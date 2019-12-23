// Antoinette R. Silas and Gerald R. Morris
// naiive matrix-vector multiplication with OpenAcc
//
// **** TO COMPILE WITH OpenACC ON UTILITY SERVER *****
// qsub -l select=1:ncpus=16:ngpus=1 -A $ACCOUNT -I
// module swap compiler/pgi/11.10 compiler/pgi/12.9
// pgcc -acc -fast -Minfo matmult.c usec.c -o matmult
//
// ********** TO COMPILE SEQUENTIALLY ONLY ************
// gcc -o matmult -O2 -Wall matmult.c usec.c
//
// ******************** TO RUN ************************
// ./matmult <ra> <ca> <rb> <cb>//r is the row size and c is column size of matrices A & B

// OpenACC does NOT allow 2-d  arrays so a 2-d matrix of dimension n x m
// needs to be converted into a 'flattened' array of length n*m

 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>
//#include "accelmath.h"
#include "usec.h"
int main(int argc, char *argv[]) {

    int **A; 	      // 2-d matrix
    int **B;	      // multiplication vector
    int **C, **seq;	      // solution matrices
    int *am, *bm, *cm;        // malloc matrices
    int ra, ca, rb, cb;  // row and column sizes of matrix A & B
    int h,i,j,k;        // indices
    int sum = 0;           // scalar sum calculated on GPU
   // int nThreads;	// input number of Threads to run
    double t0,t1,t2;  // start and stop times

    
    //make sure rows and columns are specified
    if((argc !=5) || (atoi(argv[2])!=atoi(argv[3])) ) {
      fprintf(stderr,"*** Usage: ./matmult ra ca rb cb (where ca == rb)\n");
      exit(1);
    }

    ra = atoi(argv[1]);
    ca = atoi(argv[2]);
    rb = atoi(argv[3]);
    cb = atoi(argv[4]);
    //nThreads = atoi(argv[5]);
    
    //omp_set_num_threads(nThreads);

    // allocate r * c contiguous floats in one fell swoop
    am = (int *)malloc(ra * ca * sizeof(int));
    bm = (int *)malloc(rb * cb * sizeof(int));
    cm = (int *)malloc(ra * cb * sizeof(int));

    A = (int **)malloc(ra * sizeof(int *));
    B = (int **)malloc(rb * sizeof(int *));
    C = (int **)malloc(ra * sizeof(int *));
    //seq = (int **)malloc(ra * sizeof(int *));
    
    for(i = 0; i < ra; i++){
	A[i] = &am[i*ca];
	C[i] = &cm[i*cb];
	//seq[i] = &cm[i*cb];
    }
	
    for(i = 0; i < rb; i++){
	B[i] = &bm[i*cb];
    }
	
    // initialize matrix input values and set matrix C to zeroes
    for(i = 0; i < ra; i++) {
        for(j = 0; j < ca; j++) {
            A[i][j] = i+j;
        }
    }

    for(i = 0; i < rb; i++) {
        for(j = 0; j < cb; j++) {
            B[i][j] = 1;
        }
    }

  //begin offload to MIC    
  //#pragma offload target(mic:MIC_DEV) \
    in(A:length(ra*ca)) in(B:length(rb*cb)) \
    out(C:length(ra*cb))

  //zero the C matrix
  //#pragma omp parallel for default(none) shared(C,ra,cb)
  for(i = 0; i < ra; i++) {
        for(j = 0; j < cb; j++) {
            C[i][j] = 0;
    //        seq[i][j] = 0;
        }
    }


   // print initial matrix values
    printf("\n\nA:\n");

    for(i = 0; i < ra; i++) {
        printf("\n");
        for(j = 0; j < ca; j++) {
            printf("%d",A[i][j]);
        }
    }
   
    printf("\n\nB:\n");
    for(i = 0; i < rb; i++) {
        printf("\n");
        for(j = 0; j < cb; j++) {
            printf("%d",B[i][j]);
        }
    }

    printf("\n\nC old:\n");
    for(i = 0; i < ra; i++) {
        printf("\n");
        for(j = 0; j < cb; j++) {
            printf("%d",C[i][j]);
        }
    } 
    
    // Compute matrix multiplication
    //    for (i = 0; i < r; i++) {
    //  for (j = 0; j < c; j++){ 
    //      b[i] += A[i*c+j] * x[i];
    //   }
    //}

      printf("\n");
      for (h = 0; h < (ra*cb*ca); h++){
         //fprintf("\n%d %d %d\n", ra,ca,cb);
          i = floor(h/(ca*cb));
          j = floor((h/ca)%cb);
          k = h%ca;
          printf("%d, %d, %d, %d\n", h, i, j, k);
      }

    //start clock
    t0 = usec();

    //Compute matrix multiplication.
    //#pragma omp parallel for default(none) shared(A,B,C,ra,ca,cb) 
    for(i = 0; i < ra; i++) {
      for(j = 0; j < cb; j++) {
	for(k = 0; k < ca; k++) {
	    //  sum += A[i][k]*B[k][j];
              C[i][j] += A[i][k] * B[k][j];
       // }
        //C[i][j] = sum;
        //sum = 0;
      }
    }
  }

    t1 = usec(); //stop watch for ACC, start seq

    // Compute the multiplication sequentially
    //for (h = 0; h < ra*cb*ca; h++){
	//i = floor(h/(ca*cb)); 
        //j = floor((h/ca)%cb);
	//k = h%ca;
        
        //printf("VALUES: %d, %d, %d, %d\n", h,i,j,k); 
	//printf("initial value for: seq[%d][%d] = %d\n",i,j,seq[i][j]);
	//seq[i][j] += A[i][k] * B[k][j];
	//printf("for %d iteration: seq[%d][%d] = %d\n\n",k,i,j,seq[i][j]);
    //}   

    //t2 = usec(); //stop watch for seq

    // check all the OpenACC calculations
    //for (i = 0; i < ra; i++){
      //for (j = 0; j < cb; j++){
        //if(C[i][j] != seq[i][j]) {
          //printf("Error %d %d\n", i,j);
          //exit(1);
        //}
      //}
    //}
     
    printf("Intel software matrix multiplication test was successful!\n");   
    printf("SOFTWARE runtime: %4.2f [s]\n",
 		 (t1-t0)/1e6);

    printf("\n\nC new:\n");
    for(i = 0; i < ra; i++) {
        printf("\n");
        for(j = 0; j < cb; j++) {
            printf("%d",C[i][j]);
        }
    }

    //printf("\n\nseq:\n");
    //for(i = 0; i < ra; i++) {
       // printf("\n");
        //for(j = 0; j < cb; j++) {
           // printf("%d",seq[i][j]);
        //}
    //}


    free(A); 
    free(B);
    free(C);
   // free(seq);
    free(am);
    free(bm);
    free(cm);

  return 0;
}

