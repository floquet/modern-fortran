/* http://thy.phy.bnl.gov/~creutz/z2/z2.c */

/* Z_2 lattice gauge simulation */
/* Michael Creutz <creutz@bnl.gov>     */
/* http://thy.phy.bnl.gov/~creutz/z2.c */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* the lattice is of dimensions SIZE**4  */
#define SIZE 2
int link[SIZE][SIZE][SIZE][SIZE][4]; /* last index gives link direction */

/* utility functions */
void moveup(int x[],int d) {
  x[d]+=1;
  if (x[d]>=SIZE) x[d]-=SIZE; 
  return;
}
void movedown(int x[],int d) {
  x[d]-=1;
  if (x[d]<0) x[d]+=SIZE;
  return;
}
void coldstart(){  /* set all links to unity */
  int x[4],d;
  for (x[0]=0;x[0]<SIZE;x[0]++)
    for (x[1]=0;x[1]<SIZE;x[1]++)
      for (x[2]=0;x[2]<SIZE;x[2]++)
        for (x[3]=0;x[3]<SIZE;x[3]++)
          for (d=0;d<4;d++)
	    link[x[0]][x[1]][x[2]][x[3]][d]=1;
  return;
}
/* for a random start: call coldstart() and then update once at beta=0 */

/* do a Monte Carlo sweep; return energy */
double update(double beta){
  int x[4],d,dperp,staple,staplesum;    
  double bplus,bminus,action=0.0; 
  for (x[0]=0; x[0]<SIZE; x[0]++)
    for (x[1]=0; x[1]<SIZE; x[1]++)
      for (x[2]=0; x[2]<SIZE; x[2]++)
        for (x[3]=0; x[3]<SIZE; x[3]++)
          for (d=0; d<3; d++) {
            staplesum=0;
            for (dperp=0;dperp<3;dperp++){
              if (dperp!=d){
                  printf("d = \t\t%i, dperp = \t%i\n",d,dperp); 
                /*  move around thusly:
                    dperp        6--5
                    ^            |  |
                    |            1--4
                    |            |  |
                    -----> d     2--3  */
                /* plaquette 1234 */
                  printf("1234 x = \t\t%i\t%i\t%i\t%i\n",x[0],x[1],x[2],x[3]); 
                  printf("1234 link = \t\t%i\n",link[x[0]][x[1]][x[2]][x[3]][dperp]); 
                movedown(x,dperp);
                printf("movedown \t%i\n", dperp);
                  printf("1234 link * = \t\t%i\t%i\t%i\t%i\t%i\t%i\n",x[0],x[1],x[2],x[3],d,link[x[0]][x[1]][x[2]][x[3]][d]); 
                staple=link[x[0]][x[1]][x[2]][x[3]][dperp]
                  *link[x[0]][x[1]][x[2]][x[3]][d];
                moveup(x,d);
                  printf("1234 link * = \t\t%i\t%i\t%i\t%i\t%i\t%i\n",x[0],x[1],x[2],x[3],d,link[x[0]][x[1]][x[2]][x[3]][dperp]); 
                staple*=link[x[0]][x[1]][x[2]][x[3]][dperp];  
                moveup(x,dperp);
                  printf("1234 link * = \t\t%i\t%i\t%i\t%i\t%i\n",x[0],x[1],x[2],x[3],dperp); 
                staplesum+=staple;
                  printf("staplesum = %i; staple = %i\n\n", staplesum, staple);
                /* plaquette 1456 */
                  printf("1456 link = \t\t%i\t%i\t%i\t%i\t%i\t%i\n",x[0],x[1],x[2],x[3],dperp,link[x[0]][x[1]][x[2]][x[3]][dperp]); 
                staple=link[x[0]][x[1]][x[2]][x[3]][dperp];
                moveup(x,dperp);
                  printf("1456 link * =  \t\t%i\t%i\t%i\t%i\t%i\t%i\n",x[0],x[1],x[2],x[3],d,dperp); 
                movedown(x,d);
                  printf("1456 link * =  \t\t%i\t%i\t%i\t%i\t%i\t%i\n",x[0],x[1],x[2],x[3],d,dperp); 
                staple*=link[x[0]][x[1]][x[2]][x[3]][d];
                movedown(x,dperp);
                  printf("1456 link * =  \t\t%i\t%i\t%i\t%i\t%i\t%i\n\n\n",x[0],x[1],x[2],x[3],d,dperp); 
                staple*=link[x[0]][x[1]][x[2]][x[3]][dperp];
                staplesum+=staple;
              }
	    }
            /* calculate the Boltzmann weight */
            bplus=exp(beta*staplesum);
            bminus=1/bplus;
            bplus=bplus/(bplus+bminus);
            /* the heatbath algorithm */
            if ( drand48() < bplus ){
              link[x[0]][x[1]][x[2]][x[3]][d]=1;
              action+=staplesum;
            }
            else{ 
              link[x[0]][x[1]][x[2]][x[3]][d]=-1;
              action-=staplesum;
            }
          }
  action /= (SIZE*SIZE*SIZE*SIZE*4*6);
  return 1.-action;
}

/******************************/
int main(){
  double beta, dbeta, action;
  srand48(1234L);  /* initialize random number generator */
  /* do your experiment here; this example is a thermal cycle */
  dbeta=.5;
  coldstart();
  /* heat it up */
  for (beta=1; beta>0.0; beta-=dbeta){
    action=update(beta);
    printf("%g\t%g\n",beta,action); 
  }
  printf("\n\n");
  /* cool it down */
  for (beta=0; beta<1.0; beta+=dbeta){
    action=update(beta);
    printf("%g\t%g\n",beta,action); 
  }
  printf("\n\n");
  exit(0);
}