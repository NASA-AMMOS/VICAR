#ifndef __RPC_H
#define __RPC_H

#define N_RPC  93

#define RPC_FLAG_REQUIRE 0    
#define RPC_FLAG_FALLBACK_RPC00A 1    
#define RPC_FLAG_FALLBACK_RPC00B 2    
#define RPC_FLAG_FORCE_RPC00A 3    
#define RPC_FLAG_FORCE_RPC00B 4    

#define RPC_VERSION_UNDEFINED 0    
#define RPC_VERSION_RPC00A 1    
#define RPC_VERSION_RPC00B 2    

#define RPC_LAT_OFFSET 5
#define RPC_LON_OFFSET 6
#define RPC_HGT_OFFSET 7
#define RPC_LAT_SCALE 10
#define RPC_LON_SCALE 11
#define RPC_HGT_SCALE 12

#define RPC_SAMPLE_OFFSET 4
#define RPC_LINE_OFFSET 3
#define RPC_SAMPLE_SCALE 9
#define RPC_LINE_SCALE 8


#define RPC_NS_START 53
#define RPC_DS_START 73
#define RPC_NL_START 13
#define RPC_DL_START 33


#ifdef __cplusplus
extern "C" {
#endif

int rpc_forward(int rpc_version, double *R, int n, double *llh, double *uv);
int rpc_n_forward(int rpc_version, double *R, int n, double *plh, double *uv);

#ifdef __cplusplus
}
#endif

#endif


