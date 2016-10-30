
module load cuda
module swap compiler/intel/15.0.3 compiler/gcc/4.8.5
nvcc -o CudaInfo.exe CudaInfo.cu

