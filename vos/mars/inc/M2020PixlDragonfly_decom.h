/* 
 * Dragonfly Decompression Header File
 *
 */

#ifndef DRAGONFLY_DECOM_H
#define DRAGONFLY_DECOM_H

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

int dfly_uncompress( struct dfly_output_t* input, int* output, int debug );

#define DEBUG_SIGNPOST  0x0001
#define DEBUG_GV        0x0002
#define DEBUG_LABEL     0x0004
#define DEBUG_INFILE    0x0008
#define DEBUG_BINARY    0x0020
#define DEBUG_COMP      0x0100
#define DEBUG_UNCOMP    0x0200
#define DEBUG_K_BITS    0x0400
#define DEBUG_ENC_BITS  0x0800
#define DEBUG_RAW       0x1000
#define DEBUG_PEDANTIC  0x8000

#define MAX_STR_LEN     1024
#define MAX_TOKEN_SIZE   64  // How long can a token be, for Pete's sake?

#endif //DRAGONFLY_DECOM_H

