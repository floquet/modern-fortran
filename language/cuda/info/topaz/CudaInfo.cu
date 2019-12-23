#include <stdio.h>
#include <stdlib.h>

#include <cuda.h>

void cuda_device_init(void)
    {
    int ndev;
    cudaError_t ierr;
    ierr = cudaGetDeviceCount(&ndev);
    printf("ndev = %d\n", ndev);
    if(ndev > 16) {
       printf("Error ndev too large.\n");
       return;
    }
    if(ierr != cudaSuccess) {
       printf("Error in getting device count.\n");
       return;
    }
    cudaThreadSynchronize();
    printf("There are %d GPUs.\n",ndev);
     
    for(int i=0;i<ndev;i++) {
       cudaDeviceProp pdev;
       cudaGetDeviceProperties(&pdev,i);
       cudaThreadSynchronize();
       printf("Name  : %s\n",pdev.name);
       printf("Capability  : %d %d\n",pdev.major,pdev.minor);
       printf("Memory Global: %d Mb\n",(pdev.totalGlobalMem+1024*1024)/1024/1024);
       printf("Memory Const : %d Kb\n",pdev.totalConstMem/1024);
       printf("Memory Shared: %d Kb\n",pdev.sharedMemPerBlock/1024);
       printf("Clock  : %.3f GHz\n",pdev.clockRate/1000000.0);
       printf("Processors  : %d\n",pdev.multiProcessorCount);
       printf("Cores  : %d\n",8*pdev.multiProcessorCount);
       printf("Warp  : %d\n",pdev.warpSize);
       printf("Max Thr/Blk  : %d\n",pdev.maxThreadsPerBlock);
       printf("Max Blk Size : %d %d %d\n",pdev.maxThreadsDim[0],pdev.maxThreadsDim[1],pdev.maxThreadsDim[2]);
       printf("Max Grid Size: %d %d %d\n",pdev.maxGridSize[0],pdev.maxGridSize[1],pdev.maxGridSize[2]);
    }
}

int main(int argc, char * argv[]) {

   cuda_device_init();
   return 0;
}
