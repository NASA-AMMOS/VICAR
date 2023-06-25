/*
xmln
A non-validating, ESIS generating, XML application built on top
of expat by James Clark -- http://www.jclark.com
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "expat.h"

// Output newline and tab characters as escapes
// Required both for attribute values and character data (#PCDATA)
static void SanitizeData (const char *s,int len) {
  while (len--) {
	switch (*s) {
	case 10:
	  printf("\\n");
	  break;
	case 13:
	  break;
	case 9:
    printf ("\\t");
	  break;
	case '\\':
	  printf ("\\\\");
	  break;
	default:
	  putchar (*s);
	}
	s++;
  }
}

int CompareAttributes (const void *a1,const void *a2) {
  return strcmp (*(const char **)a1,*(const char **)a2);
}

void startElement(void *userData, const char *name, const char **atts)
{
  const char **p;
  int AttributeCount;
  fprintf (stdout,"(%s\n",name);
  //**atts = name,value pairs
  // Count the number of attributes
  for (p = atts; *p; p++);

  AttributeCount = (p - atts) >> 1; // (name,value) pairs so divide by two
  if (AttributeCount > 1)
	// Sort the pairs based on the name part of the pair
	qsort ((void *)atts,AttributeCount,sizeof(char *)*2,CompareAttributes);

  while (*atts) {
	// Attribute Name
	fprintf (stdout,"A%s ",*atts);
	atts++; // Now pointing at value - can contain literal "\n" so escape
	SanitizeData(*atts,strlen(*atts));
	atts++;
	putchar('\n');
  }
}

void endElement(void *userData, const char *name)
{
  fprintf (stdout,")%s\n",name);
}

void CharacterData (void *userdata, const char *s,int len) {
  fprintf (stdout, "-");
  SanitizeData (s,len);
  putchar ('\n');
}

void ProcessingInstruction(void *userdata, const XML_Char *target, const XML_Char *data)
{
  //fprintf (stdout, "?%s %s\n",target,data);
  fprintf(stdout,"?%s ",target);
  SanitizeData(data, strlen(data));
  fprintf(stdout,"\n");
}

void UnparsedEntityDeclHandler(void *userdata,
						   const char *entityName,
						   const char *base,
						   const char *systemId,
						   const char *publicId,
						   const char *notationName) {
  fprintf (stdout,"U%s %s %s%s%s\n",entityName,notationName,systemId,
		   (publicId==(const char *)NULL?"":" "),(publicId==(const char *)NULL?"":publicId));
}

void NotationDeclHandler(void *userData,
					const XML_Char *notationName,
					const XML_Char *base,
					const XML_Char *systemId,
						 const XML_Char *publicId) {
  fprintf (stdout,"N%s %s%s%s\n",notationName,systemId,
		   (publicId==(const char *)NULL?"":" "),(publicId==(const char *)NULL?"":publicId));
}

int ExternalEntityReferenceHandler(XML_Parser parser,
					    const XML_Char *openEntityNames,
					    const XML_Char *base,
					    const XML_Char *systemId,
							  const XML_Char *publicId) {
  const char *p = openEntityNames;
  fprintf (stdout,"&");
  // Up to space is the name of the referenced entity 
  while (*p && (*p != ' ')) {
	putchar (*p);
	p++;
  }
  fprintf (stdout," %s%s%s\n",systemId,
		   (publicId==(const char *)NULL?"":" "),(publicId==(const char *)NULL?"":publicId));
  // Indicate success
  return 1;
  // Could parse referenced entity with this sort of thing?
  //XML_Parser ep = XML_ExternalEntityParserCreate(parser,openEntityNames,0);
}

static void usage () {
  fprintf (stderr,"XMLESIS XML parsing utility.\n");
  fprintf (stderr,"XML Parser by James Clark http://www.jclark.com\n");
  fprintf (stderr,"ESIS Generation by Sean Mc Grath http://www.digitome.com/sean.html\n");
}

int process_file (FILE *input,const char *filestring) {
  // Create an XML Parser
  XML_Parser parser = XML_ParserCreate(NULL);
  static char buf[BUFSIZ];
  int done;

  // Establish Event Handlers
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler (parser, CharacterData);
  XML_SetProcessingInstructionHandler(parser, ProcessingInstruction);
  XML_SetUnparsedEntityDeclHandler(parser, UnparsedEntityDeclHandler);
  XML_SetNotationDeclHandler(parser, NotationDeclHandler);
  XML_SetExternalEntityRefHandler(parser, ExternalEntityReferenceHandler);

  do {
    size_t len = fread(buf, 1, sizeof(buf), input);
    done = len < sizeof(buf);
    if (!XML_Parse(parser, buf, len, done)) {
      fprintf(stderr,
	      "File:'%s' Error:'%s' on line '%d'\n",
			  filestring,
			  XML_ErrorString(XML_GetErrorCode(parser)),
			  XML_GetCurrentLineNumber(parser));
      return 1;
    }
  } while (!done);
  XML_ParserFree(parser);
  return 0;
}

int main(int argc,const char *argv[])
{
  FILE *input;

  if ((argc > 1) &&
	  (
	   (strcmp(argv[1],"-h") == 0) ||
	   (strcmp(argv[1],"-H") == 0) ||
	   (strcmp(argv[1],"-?") == 0) ||
	   (strcmp(argv[1],"-help") == 0) 
	   )) {
	usage();
	exit(0);
  }
  if (argc == 1) {
	// Uncomment the following fprintf if you would like to
	// see a message on standard error when input is being
	// read from standad input rather than from a file 
	//fprintf (stderr,"Reading from standard input\n");
	process_file(stdin,"<stdin>");
  }
  else {
	for (++argv; argc>1; argc--,argv++) {
	  // Uncomment the following fprintf if you want to see
	  // filenames appearing on standard error.
	  //fprintf (stderr,"File:'%s'\n",*argv);
	  input = fopen (*argv,"r");
	  if (input == (FILE *)NULL) {
		fprintf (stderr,"Cannot open input file '%s'\n",*argv);
	  }
	  else {
		process_file (input,*argv);
		fclose (input);
	  }
	}
  }
  return 0;
}
