#include <iostream>
using namespace std;

#include "MiniHeader.h"
#include "zvproto.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>





int MiniHeader::parse_mini_header(unsigned char *miniHeader_, int isZstack)
{

        //Parsing the mini-header
        this->thumbnail_ = (miniHeader_[0]&0x80)>>7;
        cout << "thumbnail: " << (unsigned int)this->thumbnail_ << endl;
        this->product_id_ = (miniHeader_[0]&127)<<24 | miniHeader_[1]<<16 | miniHeader_[2]<<8 | miniHeader_[3];
        cout << "product_id: " << (unsigned int)this->product_id_ << endl;

        this->magic0_ = miniHeader_[4]<<24 | miniHeader_[5]<<16 | miniHeader_[6]<<8 | miniHeader_[7];
        printf("magic0: %x\n",this->magic0_);
        if(this->magic0_!=0xFF00F0CA){
            const size_t msgLen = 256;
            char msg[msgLen];
            snprintf(msg,msgLen,"MSLMMMCamEdr::WARNING magic0 does not equal 0xFF00F0CA");
	    zvmessage(msg, "");
	    return 0;
	}

        this->sclk_ = miniHeader_[8]<<24 | miniHeader_[9]<<16 | miniHeader_[10]<<8 | miniHeader_[11];
        cout << "sclk: " << this->sclk_ << endl;

        this->vflush_ = miniHeader_[12]<<8 | miniHeader_[13];
        cout << "vflush: " << this->vflush_ << endl;

        this->mode_ = miniHeader_[14]<<8 | miniHeader_[15];
        cout << "mode: " << this->mode_ << endl;
      
	// Parse the 6*32 bits of command (Zstack/depth differs from others)

	this->isZstack_ = isZstack;
        if(!isZstack_){

            this->filter_ = miniHeader_[16];
            cout << "filter: " << (unsigned int)this->filter_ << endl;
            this->exposure_ = ((float)(miniHeader_[17]<<16 | miniHeader_[18]<<8 | miniHeader_[19]));
            cout << "exposure: " << this->exposure_ << endl;

            this->window_x_ = miniHeader_[20];
            cout << "window_x: " << (unsigned int)this->window_x_ << endl;
            this->window_y_ = miniHeader_[21];
            cout << "window_y: " << (unsigned int)this->window_y_ << endl;
            this->width_ = miniHeader_[22];
            cout << "width(miniheader): " << (unsigned int)this->width_ << endl;
            this->height_ = miniHeader_[23];
            cout << "height: " << (unsigned int)this->height_ << endl;

            this->autofocus_ = miniHeader_[24]<<24 | miniHeader_[25]<<16 | miniHeader_[26]<<8 | miniHeader_[27];
            cout << "autofocus: " << (unsigned int)this->autofocus_ << endl;

            this->initial_position_ = (miniHeader_[24]<<8 | miniHeader_[25])>>1;
            cout << "initial_position: " << this->initial_position_ << endl;
            this->step_size_ = (miniHeader_[25]&0x1)<<9 | miniHeader_[26]<<1 | (miniHeader_[27]&0x80)>>7;
            cout << "step_size: " << this->step_size_ << endl;
            this->af_n_steps_ = (miniHeader_[27]&0x7E)>>1;
            cout << "af_n_steps: " << (unsigned int)this->af_n_steps_ << endl;
            this->zstack_ = miniHeader_[27]&0x1;
            cout << "zstack: " << (unsigned int)this->zstack_ << endl;

            this->autoexposure_ = miniHeader_[28]<<24 | miniHeader_[29]<<16 | miniHeader_[30]<<8 | miniHeader_[31];
            cout << "autoexposure: " << this->autoexposure_ << endl;
            this->target_dn_ = miniHeader_[28];
            cout << "target_dn: " << (unsigned int)this->target_dn_ << endl;
            this->fraction_ = miniHeader_[29];
            cout << "fraction: " << (unsigned int)this->fraction_ << endl;
            this->early_termination_ = miniHeader_[30];
            cout << "early_termination: " << (unsigned int)this->early_termination_ << endl;
            this->ae_n_steps_ = miniHeader_[31]; 
            cout << "ae_n_steps: " << (unsigned int)this->ae_n_steps_ << endl;
          
	}

	// Zstacks

        else {
      
            this->filter_ = 0;
            this->exposure_ = 0;
            
            this->start_image_id_ = miniHeader_[24]<<24 | miniHeader_[25]<<16 | miniHeader_[26]<<8 | miniHeader_[27];
            cout << "start_image_id_: " << (unsigned int)this->start_image_id_ << endl;
            this->exposure_count_ = miniHeader_[28];
            cout << "exposure_count_: " << (unsigned int)this->exposure_count_ << endl;
            this->z_stack_parms_ = miniHeader_[29]<<16 | miniHeader_[30]<<8 | miniHeader_[31];
            cout << "z_stack_parms_: " << (unsigned int)this->z_stack_parms_ << endl;
      
        }

        this->compression_[0] = (unsigned char)miniHeader_[32];
        this->compression_[1] = (unsigned char)miniHeader_[33];
        this->compression_[2] = (unsigned char)miniHeader_[34];
        this->compression_[3] = (unsigned char)miniHeader_[35];
        this->compression_[4] = (unsigned char)miniHeader_[36];
        this->compression_[5] = (unsigned char)miniHeader_[37];
        this->compression_[6] = (unsigned char)miniHeader_[38];
        this->compression_[7] = (unsigned char)miniHeader_[39];
        printf("compression: %x %x %x %x %x %x %x %x\n",this->compression_[0],this->compression_[1],this->compression_[2],
        this->compression_[3],this->compression_[4],this->compression_[5],this->compression_[6],
        this->compression_[7]);
          
        if(this->compression_[3] == 0 && this->compression_[2] == 0XFF)
           isLossless_ = true;
      
      
        this->camera_status_ = miniHeader_[40];
        cout << "camera_status: " << (unsigned int)this->camera_status_ << endl;
        this->sys_serial_no_ = miniHeader_[41]<<16 | miniHeader_[42]<<8 | miniHeader_[43];
        cout << "sys_serial_no: " << this->sys_serial_no_ << endl;

        this->mech_[0] = miniHeader_[44];
        this->mech_[1] = miniHeader_[45];
        this->mech_[2] = miniHeader_[46];
        this->mech_[3] = miniHeader_[47];
        this->mech_[4] = miniHeader_[48];
        this->mech_[5] = miniHeader_[49];
        this->mech_[6] = miniHeader_[50];
        this->mech_[7] = miniHeader_[51];
        printf("mech: %x%x%x%x%x%x%x%x\n",this->mech_[0],this->mech_[1],this->mech_[2],
          this->mech_[3],this->mech_[4],this->mech_[5],this->mech_[6],
          this->mech_[7]);

	// Note, per K. Paris:
	// We're using the same bytes for the information, but note that
	// they're not in 0-1-2 order in the mini header:
	//
	// focus = bytes 44-47 (mech_0)
	// zoom = bytes 48-49 (mech_2)
	// filter = bytes 50-51 (mech_1)
	//
	// Here's the text of the Common Command Set document:
	// (note that the allocation for the mechanisms is given as 2*32bits
	// and the data are big-endian)
	// "mech Positions of the three mechanisms for this product; the
	// positions of mechanisms 1 and 2 are packed into the second word
	// with mechanism 2 in the high 16 bits and mechanism 1 in the low
	// 16 bits (for backwards compatibility.) "


        this->mech0_ = miniHeader_[44]<<24 | miniHeader_[45]<<16 | miniHeader_[46]<<8 | miniHeader_[47];
        cout << "mech0: " << (unsigned int) this->mech0_ << endl;
        this->mech2_ = miniHeader_[48]<<8 | miniHeader_[49];
        cout << "mech1: " << (unsigned short) this->mech1_ << endl;
        this->mech1_ = miniHeader_[50]<<8 | miniHeader_[51];
        cout << "mech2: " << (unsigned short) this->mech2_ << endl;

        this->dc_offset_ = miniHeader_[52]<<24 | miniHeader_[53]<<16 | miniHeader_[54]<<8 | miniHeader_[55];
        cout << "dc_offset: " << this->dc_offset_ << endl;

        this->initsize_ = miniHeader_[56]<<24 | miniHeader_[57]<<16 | miniHeader_[58]<<8 | miniHeader_[59];
        cout << "initsize: " << this->initsize_ << endl;

        this->magic1_ = miniHeader_[60]<<24 | miniHeader_[61]<<16 | miniHeader_[62]<<8 | miniHeader_[63];
        printf("magic1: %x\n",this->magic1_);
        if(this->magic1_!=0x1010CC28){
            const size_t msgLen = 256;
            char msg[msgLen];
            snprintf(msg,msgLen,"MSLMMMCamEdr::WARNING magic1 does not equal 0x1010CC28");
	    zvmessage(msg, "");
	    return 0;
        }
      
      
        //Use mini-header 4th byte for compression field
        this->comp_quality_ = this->compression_[3];
        cout << "mini header comp_quality: " << this->comp_quality_ << endl;
      
      cout << "isLossless_(from mini-header): " << (bool)isLossless_ << endl;
      


//????!!!!  Is this right?
//        if(prodType.find("Thumbnail")!=string::npos)
//           isLossless_ = false;
//
//      cout << "isLossless_: " << (bool)isLossless_ << endl;
            
      if(this->comp_quality_ == 101)
         cout << "Lossless compression found!!!" << endl;
      if(this->comp_quality_ == -101)
         cout << "Deferred Lossless compression found!!!" << endl;
      
    return 1;
}


