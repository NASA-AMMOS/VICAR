/////////////////////////////////////////////////////////////////////////////
//  merinverter.cc
//////////////////////////////////////////////////////////////////////////////
#include <cstdio>
#include <iostream>
//#include <fstream>
#include <string>
#include <string.h>
#include <stdlib.h>
using namespace std;
#include <time.h>
#include <errno.h>
#include <sys/types.h>
#include <limits.h>
#include <math.h>
#include "zvproto.h"
#include "vicmain_c"
#include "lbl_image_data.h"
#include "lbl_identification.h"
#include "lbl_pdshistory.h"
#include "lbl_instrument_state.h"
#include "rts_time.h"

#ifndef MAXSTRLEN
#define MAXSTRLEN       256
#endif

#ifndef MAXSIZE
#define MAXSIZE         1024
#endif

#ifndef MAX8BITDN
#define MAX8BITDN       256
#endif

#ifndef MAX12BITDN      
#define MAX12BITDN      4096
#endif

void readEDR         (const char*, short *data, int &, int &, int &, int &, char *);
void readInverseLUT  (const char*, short *lut);
void writeFile       (const char*, const char*, short *data, int, int, int, short *inv_lut, 
                      const char*, const char*);
void calculateStats  (short *data, u_int &, u_int &, float &, float &, double &, u_int &, int, int);
void autoInvert      (short *lut);

char logFname[MAXSTRLEN];
bool mipl, edr;

//............................................................................
void main44()
{
    char inFname[MAXSTRLEN],
         inverseLutFname[MAXSTRLEN],
         outFname[MAXSTRLEN],
         tname[MAXSTRLEN],
         tname1[MAXSTRLEN],
         prodName[MAXSTRLEN],
         prodId[MAXSTRLEN],
         insSclk[12],
         frame[3],
         posSiteSeqEye[11],
         who[1],
         outDir[MAXSTRLEN];

    inverseLutFname[0] = '\0';

    char *ptr, lutVersion[10];

    short *imgData, *inverseLut, version;

    int nl, ns, nb, unit, count;
    int autoFlag = 0;
    int shiftFlag = 0;

    zvmessage("MERINVERTER version 2020-02-04", "");

    zvp("IN",                inFname,            &count);
    zvp("OUT_DIR",           outDir,             &count);
    if (zvptst("MIPL")) {
        who[0] = 'M';
        mipl = true;
    }
    else {
        who[0] = 'V';
        mipl = false;
    }
    version = 1;

    if (zvptst("EDR"))
        edr = true;
    else
        edr = false;
    
    strcpy(tname, inFname);
    if ((ptr = strrchr(tname, '/'))) ptr++;   //remove any dir path from fname.
    else ptr = tname;

    strcpy (prodName, ptr);
    snprintf(prodId, MAXSTRLEN, "%s", strtok(prodName, "."));
    ptr = prodId;
    strncpy(insSclk, ptr, 11);
    insSclk[11] = '\0';
    strncpy(frame, (ptr+11), 3);
    strncpy(posSiteSeqEye, (ptr+14), 11);

    if (!(!strncasecmp(frame, "eff", 3) ||
        !strncasecmp(frame, "esf", 3) ||
        !strncasecmp(frame, "edn", 3) ||
        !strncasecmp(frame, "eth", 3))) {
        zvmessage("Invalid EDR type to perform INVERSE LUT.", "");
        zabend();
    }

    if (!edr) {
        if (!strncasecmp(frame, "eff", 3))
            strncpy(frame, "ILF", 3);
        else if (!strncasecmp(frame, "esf", 3))
            strncpy(frame, "ISF", 3);
        else if (!strncasecmp(frame, "edn", 3))
            strncpy(frame, "INN", 3);
        else if (!strncasecmp(frame, "eth", 3))
            strncpy(frame, "ITH", 3);
    }

    strcpy(tname1, insSclk);
    strncat(tname1, frame, 3);
    strncat(tname1, posSiteSeqEye, 11);
    snprintf(tname1, MAXSTRLEN, "%s%c", tname1, who[0]);
    snprintf(tname1, MAXSTRLEN, "%s%1d", tname1, version);

    snprintf(outFname, MAXSTRLEN, "%s/%s.VIC", outDir, tname1);

    imgData = new short[MAXSIZE*MAXSIZE];
    memset(imgData, 0, MAXSIZE*MAXSIZE*sizeof(short));
    readEDR (inFname, imgData, nl, ns, nb, unit, lutVersion);

    if (!strncasecmp(lutVersion, "lut1", 4)) 
        strcpy(inverseLutFname, "ILUT1.txt");
    else if (!strncasecmp(lutVersion, "lut2", 4)) 
        strcpy(inverseLutFname, "ILUT2.txt");
    else if (!strncasecmp(lutVersion, "lut3", 4)) 
        strcpy(inverseLutFname, "ILUT3.txt");
    else if (!strncasecmp(lutVersion, "lut4", 4))
        strcpy(inverseLutFname, "ILUT4.txt");
    else if (!strncasecmp(lutVersion, "lut5", 4))
        strcpy(inverseLutFname, "ILUT5.txt");
    else if (!strncasecmp(lutVersion, "autoshift", 9))
        autoFlag = 1;
    else if (!strncasecmp(lutVersion, "msb_bit", 7))
        shiftFlag = 1;
    else {
        char temp[256];
        snprintf(temp, 256, "Invalid LUT value - %s. Can NOT perform Inverse LUT.", lutVersion);
        zvmessage(temp, "");
        zabend();
    }

    inverseLut = new short[MAX8BITDN];
    memset(inverseLut, 0, MAX8BITDN*sizeof(short));
    
    if (!shiftFlag) {
        if (autoFlag)
           autoInvert(inverseLut);
        else 
           readInverseLUT (inverseLutFname, inverseLut);
    }

    // created VICAR label EDRs
    if (shiftFlag) {
        writeFile (inFname, outFname, imgData, nl, ns, unit, NULL, tname1, lutVersion);
    }
    else {
        writeFile (inFname, outFname, imgData, nl, ns, unit, inverseLut, tname1, NULL);
    }

    if (autoFlag) {
        char msg[MAXSTRLEN];
        snprintf(msg,MAXSTRLEN,"Linear 8 to 12 bits conversion has been applied for AUTOSHIFT.");
        zvmessage(msg,"");
    }
    delete [] imgData;
}
//............................................................................
void readEDR (const char *fname, short *data, int &nl, int &ns, int &nb, int &unit, char *lutVersion)
{
    int status, in_unit;
    char msg[MAXSTRLEN];

    status = zvunit(&in_unit, "O", 1, "U_NAME", fname, NULL);
    status = zvopen(in_unit, "OP", "READ", "U_FORMAT", "HALF","OPEN_ACT", "SA", NULL);
    if (status != 1) {
        snprintf(msg, MAXSTRLEN, "Error opening EDR - %s", fname);
        zvmessage(msg, "");
        zabend();
    }

    zvget(in_unit, "NL", &nl, "NS", &ns, "NB", &nb, NULL);

    zlget(in_unit, "PROPERTY", "SAMPLE_BIT_MODE_ID", lutVersion, "PROPERTY",
          "INSTRUMENT_STATE_PARMS", NULL);

    for (int j=0; j < nl; j++) {
        zvread(in_unit, &data[j*ns], "LINE", j+1, "NSAMPS", ns, NULL);
    } 

    zvclose(in_unit, NULL);
    unit = in_unit;
}

//............................................................................
void writeFile(const char *inFname, const char *outFname, short *data, 
               int nl, int ns, int out_unit, short *inverse_lut, 
               const char *prodId, const char *lutVersion)
{
    int status;
    u_int min = UINT_MAX, max = 0;
    u_int checkSum = 0;
    float mean = 0.0, median = 0.0;
    double stdDev = 0.0;

    if (inverse_lut!=NULL) {
        for (int i=0; i<nl*ns; i++) {
            data[i] = inverse_lut[data[i]];
        }
    }
    else {
        int bitsShift = 0;
        char tmpLut[MAXSTRLEN]; 
        strcpy(tmpLut, lutVersion);
        char *ptr = (tmpLut+7);
        bitsShift = atoi(ptr);

        for (int i=0; i<nl*ns; i++) {
            data[i] = data[i]<<(bitsShift-8+1);
        }
    }

    // calculate statistic keywords
    calculateStats (data, min, max, mean, median, stdDev, checkSum, nl, ns);

    zvselpiu(out_unit);    // transfer labels
    zvunit(&out_unit, "writeFile", 1, "U_NAME", outFname, NULL);
    zvopen(out_unit, "OP", "WRITE", "METHOD", "SEQ", "O_FORMAT", "HALF",
           "U_FORMAT", "HALF", "U_NL", nl, "U_NS", ns, NULL);

    // replace the values of keywords in image_data property
    LblImageData_typ imgData;
    memset(&imgData, 0, sizeof(LblImageData_typ));
    status = LblImageData(out_unit, LBL_READ, &imgData, 1);

    imgData.Mean.Value = mean;
    imgData.Mean.Valid = LBL_VALID;
    imgData.Median.Value = median;
    imgData.Median.Valid = LBL_VALID;
    imgData.Maximum.Value = max;
    imgData.Maximum.Valid = LBL_VALID;
    imgData.Minimum.Value = min;
    imgData.Minimum.Valid = LBL_VALID;
    imgData.StandardDeviation.Value = stdDev;
    imgData.StandardDeviation.Valid = LBL_VALID;
    imgData.Checksum.Value = checkSum;
    imgData.Checksum.Valid = LBL_VALID;
    strcpy(imgData.SampleBitMask.Value, "2#0000111111111111#");
    imgData.SampleBitMask.Valid = LBL_VALID;

    status = LblImageData(out_unit, LBL_AUGMENT, &imgData, 1);
    if (status)
        zvmessage((char *)LblErrorMessage(), "Image_Data");

    LblIdentification_typ id;
    memset(&id, 0, sizeof(LblIdentification_typ));
    status = LblIdentification(out_unit, LBL_READ, &id, 1);

    if (zvptst("VERSION")) {
        sprintf(id.ProductCreationTime.Value, "%sZ", rts_utc_time());
        id.ProductCreationTime.Valid = LBL_VALID;
    }
    strcpy(id.ProductId.Value, prodId);
    id.ProductId.Valid = LBL_VALID;
    if (!mipl) {
        sprintf(id.ProducerInstitutionName.Value, "%s", "SSV Team at JPL");
        id.ProducerInstitutionName.Valid = LBL_VALID;
    }

    status = LblIdentification(out_unit, LBL_AUGMENT, &id, 1);
    if (status)
        zvmessage((char *)LblErrorMessage(), "Identification");

    if (zvptst("VERSION")) {
        LblPdsHistory_typ his;
        memset(&his, 0, sizeof(LblPdsHistory_typ));
        status = LblPdsHistory(out_unit, LBL_READ, &his, 1);

        if (!mipl) {
            sprintf(his.ProcessingHistoryText.Value, "%s %s",
                    his.ProcessingHistoryText.Value, 
                    ";;; SSV EDR/RDR created using MERINVERTER");
            his.ProcessingHistoryText.Valid = LBL_VALID;
        }
        sprintf(his.SoftwareName.Value, "%s", "MERINVERTER");
        his.SoftwareName.Valid = LBL_VALID;
        sprintf(his.SoftwareVersionId.Value, "%s", "V1.0 Dec-11-2003");
        his.SoftwareVersionId.Valid = LBL_VALID;

        status = LblPdsHistory(out_unit, LBL_AUGMENT, &his, 1);
        if (status)
            zvmessage((char *)LblErrorMessage(), "PDS_History");
    }

    LblInstrumentState_typ inst;
    memset(&inst, 0, sizeof(LblInstrumentState_typ));
    status = LblInstrumentStateParms(out_unit, LBL_READ, &inst, 1);

    if (strcasecmp(inst.SampleBitMethod.Value, "hardware") == 0) 
        sprintf(inst.SampleBitMethod.Value, "%s", "HARDWARE_INVERTED");
    else if (strcasecmp(inst.SampleBitMethod.Value, "software") == 0) 
        sprintf(inst.SampleBitMethod.Value, "%s", "SOFTWARE_INVERTED");
    else 
        sprintf(inst.SampleBitMethod.Value, "%s", "NONE");
    inst.SampleBitMethod.Valid = LBL_VALID;

    status = LblInstrumentStateParms(out_unit, LBL_AUGMENT, &inst, 1);
    if (status)
        zvmessage((char *)LblErrorMessage(), "Instrument_State");

    for (int j=0; j < nl; j++) {
        zvwrit(out_unit, &data[j*ns], "LINE", j+1, "NSAMPS", ns, NULL);
    }

    zvclose(out_unit, NULL);
    printf("Created INVERSE-LUT product: %s\n", outFname);
}

//............................................................................
void readInverseLUT(const char *inverseLutFname, short *inverseLut)
{
    char msg[MAXSTRLEN];
    char path_list[1024], orig_path_list[1024]; // max length of path string
    char *path_element[128];			// max # of path elements
    char pathname[1024];
    int count, status, index = 0;
    short value = 0;

    int def;
    zvparmd("CONFIG_PATH", orig_path_list, &count, &def, 1, sizeof(orig_path_list)-1);

    if (count == 0)			// no path, abend
        zabend();

    // Expand environment variables (and ~ if the first character)
    status = zvfilename(orig_path_list, path_list, sizeof(path_list)-1);
    if (!status) {
        zvmessage("Syntax error or undefined variable in config path","");
        zabend();
    }

    // Find the :'s in the string, and replace them with nulls.	 Build up
    // the path_element pointer array.

    int num_paths = 0;
    char *p = path_list;

    path_element[num_paths++] = p;
    while ((p = strchr(p, ':'))) {
        *p++ = '\0';
        if (*p)
            path_element[num_paths++] = p;
    }

    // Now search through the path until we find one we like
    FILE* inf = NULL;
    for (int i=0; i < num_paths; i++) {
        strcpy(pathname, path_element[i]);
        strcat(pathname, "/");
        strcat(pathname, inverseLutFname);

        inf = fopen(pathname, "r");
        if (inf!=NULL) { // found a file
            break;
        }
    }

    if (inf==NULL) {
        snprintf(msg, MAXSTRLEN, "Error opening file %s", inverseLutFname);
        zvmessage(msg, "");
        zabend();               // error!!
    }

    char tmpLine[1024];
    while (fgets(tmpLine, 1024, inf)) {
        char *pch = strtok(tmpLine, " ");
        if (pch != NULL) 
           index = atoi(pch);
        else
           cout << "Error reading a value from " << pathname << endl;
        pch = strtok(NULL, " ");
        if (pch != NULL) 
           value = atoi(pch);
        else 
           cout << "Error reading a value from " << pathname << endl;
        inverseLut[index] = value;
    }
    fclose(inf);
}
//
// This routine perform 8-12 bits conversion by shifting 4bits
// and stores in inverseLut array.
// 
void autoInvert (short *inverseLut)
{
    for (int i=0; i<256; i++) {
        inverseLut[i] = i<<4;
    }
}

//............................................................................
//  This routine calculates the Max, Min, Mean, Median and Standard
//  Deviation.  This routine is for IMG, TMB, and REF types.
//............................................................................
void calculateStats(short * pixelBuffer, u_int &minPixel, u_int &maxPixel, 
                    float &meanPixel, float &medianPixel, double &stdDevPixel,
                    u_int &checkSum, int nl, int ns)
{
   double bufferSum = 0;
   int   pixelOut,
         idx,
        *histogram;
   u_long pixelSum = 0;
   u_int histIdx,
         maxHistSize;
   double interimStep,
          variance = 0.0;

   pixelOut = nl * ns ;
   if (!pixelOut) return;

   maxHistSize = MAX12BITDN;

   histogram = new int[maxHistSize];
   memset(histogram,0,maxHistSize*sizeof(int));

   for (idx=0; idx<pixelOut; idx++) {
      histIdx = (u_long)pixelBuffer[idx];

      if (histIdx < maxHistSize && histIdx >= 0) {
         histogram[histIdx]++;
         checkSum += (u_int)pixelBuffer[idx];
      }

      if ((u_int)pixelBuffer[idx] > maxPixel)
         maxPixel = (u_int)pixelBuffer[idx];

      if ((u_int)pixelBuffer[idx] < minPixel)
         minPixel = (u_int)pixelBuffer[idx];

      bufferSum += (double)pixelBuffer[idx];
   }

   meanPixel = bufferSum / pixelOut;

   if (pixelOut > 1) {
      for (idx=0; idx<maxHistSize; idx++) {
         pixelSum += histogram[idx];
         // Check for Median Value
         if (medianPixel==0.0 && pixelSum >= (pixelOut / 2))
            medianPixel = idx;

         // Calculate Variance
         interimStep = (double)idx - (double)meanPixel;
         variance += (interimStep * interimStep * (double)histogram[idx]);
      }
      variance = variance / (double)pixelOut;
      stdDevPixel = sqrt(variance);
   }

   return;
}

