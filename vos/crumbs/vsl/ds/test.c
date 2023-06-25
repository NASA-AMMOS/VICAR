#include <stdio.h>
/*#include "define.h"*/

main()
{
int sockfd;
int band, res;
int b[20];
long line, sample;
char *data;

sockfd = dsOpen("ds://fa/hermes/sl9a/bpl/bill/landers/OI");

fprintf(stderr, "requestHeader return value = %d\n",
requestHeader(sockfd, &(b[0]), &(b[1]), &(b[2]), &(b[3]), &(b[4]),
			&(b[5]), &(b[6])));
fprintf(stderr, "Values returned = %d  %d  %d  %d  %d  %d  %d\n",
b[0], b[1], b[2], b[3], b[4], b[5], b[6]);

fprintf(stderr, "Enter band(0-2), level, line, sample\n");
scanf("%d %d %d %d", &band, &res, &line, &sample);
fprintf(stderr, "TEST: Read : %d %d %d %d\n", band, res, line, sample);
while (band != -1)
  {
  fprintf(stderr, "Value = %d\n",
	requestValue(sockfd, band, res, line, sample, &data));
  scanf("%d %d %d %d", &band, &res, &line, &sample);
  fprintf(stderr, "Read : %d %d %d %d", band, res, line, sample);
  }

printf("Closing ... return value is %d\n", dsClose(sockfd));
}
