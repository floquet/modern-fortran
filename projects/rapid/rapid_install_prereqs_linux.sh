./._rapid_install_prereqs_linux_v2.sh                                                               000644  150261  000024  00000000302 12632156504 020736  0                                                                                                    ustar 00cedavid                         staff                           000000  000000                                                                                                                                                                             Mac OS X            	   2   �      �                                      ATTR       �   �   *                  �   *  com.apple.quarantine q/0001;5668dd44;Microsoft\x20Outlook.app;                                                                                                                                                                                                                                                                                                                               rapid_install_prereqs_linux_v2.sh                                                                   000644  150261  000024  00000006610 12632156504 020374  0                                                                                                    ustar 00cedavid                         staff                           000000  000000                                                                                                                                                                         #!/bin/bash
#*******************************************************************************
#rapid_install_prereqs_linux_v2.sh
#*******************************************************************************

#Purpose:
#This shell script installs programs required for RAPID.
#Author:
#Alan D. Snow and Cedric H. David, 2015, based on tutorial by Cedric H. David


#*******************************************************************************
#Before installing
#*******************************************************************************
#Make sure this file has execute privileges.
# $ chmod u+x rapid_install_prereqs.sh
#
#Compilers for C, C++ and FORTRAN are needed in order to install the libraries
#used by RAPID. Here we use the GNU Compiler Collection. Make all necessary
#compilers are installed by executing:
# $ which gcc
# $ which g++
# $ which gfortran
#
#If one or more of the compilers is missing, execute:
# $ apt-get install g++ gfortran (Debian)
# $ yum install g++ gfortran (Red Hat)


#*******************************************************************************
#Installation directory
#*******************************************************************************
INSTALLZ_DIR=$HOME/installz
#Update the location of the installation directory as you wish, but do not move
#anything after running this script, or do so at your own risks!


#*******************************************************************************
#Main script
#*******************************************************************************
mkdir -p $INSTALLZ_DIR

#-------------------------------------------------------------------------------
#netCDF
#-------------------------------------------------------------------------------
cd $INSTALLZ_DIR
wget -nc "http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-3.6.3.tar.gz"
tar -xzf netcdf-3.6.3.tar.gz
mkdir -p netcdf-3.6.3-install
cd netcdf-3.6.3
./configure CC=gcc CXX=g++ FC=gfortran --prefix=$INSTALLZ_DIR/netcdf-3.6.3-install
make check > check.log
make install > install.log

#-------------------------------------------------------------------------------
#Installing PETSc 3.6.2
#-------------------------------------------------------------------------------
cd $INSTALLZ_DIR
wget "http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-3.6.2.tar.gz"
tar -xf petsc-3.6.2.tar.gz
cd petsc-3.6.2
./configure PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx --download-fblaslapack=1 --download-mpich=1 --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --with-clanguage=cxx --with-debugging=0
make PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx all
make PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx test

#-------------------------------------------------------------------------------
#Exporting environment variables
#-------------------------------------------------------------------------------
export TACC_NETCDF_LIB=$INSTALLZ_DIR/netcdf-3.6.3-install/lib
export TACC_NETCDF_INC=$INSTALLZ_DIR/netcdf-3.6.3-install/include
export PETSC_DIR=$INSTALLZ_DIR/petsc-3.6.2
export PETSC_ARCH='linux-gcc-cxx'

#-------------------------------------------------------------------------------
#Exporting directories with library-related executables to $PATH
#-------------------------------------------------------------------------------
export PATH=$PATH:/$PETSC_DIR/$PETSC_ARCH/bin
export PATH=$PATH:$INSTALLZ_DIR/netcdf-3.6.3-install/bin
                                                                                                                        ./._rapid_install_prereqs_mac.sh                                                                    000644  150261  000024  00000000302 12575563450 017740  0                                                                                                    ustar 00cedavid                         staff                           000000  000000                                                                                                                                                                             Mac OS X            	   2   �      �                                      ATTR       �   �   *                  �   *  com.apple.quarantine q/0001;55f6e344;Microsoft\x20Outlook.app;                                                                                                                                                                                                                                                                                                                               rapid_install_prereqs_mac.sh                                                                        000644  150261  000024  00000006117 12575563450 017400  0                                                                                                    ustar 00cedavid                         staff                           000000  000000                                                                                                                                                                         #!/bin/bash
#*******************************************************************************
#rapid_install_prereqs_mac.sh
#*******************************************************************************

#Purpose:
#This shell script installs programs required for RAPID. 
#Author:
#Alan D. Snow and Cedric H. David, 2015, based on tutorial by Cedric H. David


#*******************************************************************************
#Before installing 
#*******************************************************************************
#Make sure this file has execute privileges.  
# $ chmod u+x rapid_install_prereqs.sh 
#
#Compilers for C, C++ and FORTRAN are needed in order to install the libraries 
#used by RAPID. Here we use the GNU Compiler Collection. Make all necessary 
#compilers are installed by executing:
# $ which gcc
# $ which g++
# $ which gfortran
#
#If one or more of the compilers is missing, execute:
# $ brew install gcc

#*******************************************************************************
#Installation directory
#*******************************************************************************
INSTALL_DIR=$HOME/installz
#Update the location of the installation directory as you wish, but do not move 
#anything after running this script, or do so at your own risks!


#*******************************************************************************
#Main script
#*******************************************************************************
mkdir -p $INSTALL_DIR

#-------------------------------------------------------------------------------
#netCDF
#-------------------------------------------------------------------------------
cd $INSTALL_DIR
curl -o netcdf-3.6.3.tar.gz "http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-3.6.3.tar.gz"
tar -xzf netcdf-3.6.3.tar.gz
mkdir -p netcdf-3.6.3-install
cd netcdf-3.6.3
./configure CC=gcc CXX=g++ FC=gfortran --prefix=$INSTALL_DIR/netcdf-3.6.3-install 
make check > check.log
make install > install.log

#-------------------------------------------------------------------------------
#PETSc
#-------------------------------------------------------------------------------
cd $INSTALL_DIR
curl -o petsc-3.3-p7.tar.gz "http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-3.3-p7.tar.gz"
tar -xzf petsc-3.3-p7.tar.gz
cd petsc-3.3-p7
./configure PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx --download-f-blas-lapack=1 --download-mpich=1 --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --with-clanguage=cxx --with-debugging=0
make PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx all
make PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx test

#-------------------------------------------------------------------------------
#TAO
#-------------------------------------------------------------------------------
cd $INSTALL_DIR
curl -o tao-2.1-p2.tar.gz "http://www.mcs.anl.gov/research/projects/tao/download/tao-2.1-p2.tar.gz"
tar -xzf tao-2.1-p2.tar.gz
cd tao-2.1-p2
make TAO_DIR=$PWD PETSC_DIR=$INSTALL_DIR/petsc-3.3-p7 PETSC_ARCH=linux-gcc-cxx all > make.log
make TAO_DIR=$PWD PETSC_DIR=$INSTALL_DIR/petsc-3.3-p7 PETSC_ARCH=linux-gcc-cxx tao_testfortran > fortran.log
                                                                                                                                                                                                                                                                                                                                                                                                                                                 ./._rapid_install_prereqs_cygwin.sh                                                                 000644  150261  000024  00000000302 12575563524 020502  0                                                                                                    ustar 00cedavid                         staff                           000000  000000                                                                                                                                                                             Mac OS X            	   2   �      �                                      ATTR       �   �   *                  �   *  com.apple.quarantine q/0001;55f6e32a;Microsoft\x20Outlook.app;                                                                                                                                                                                                                                                                                                                               rapid_install_prereqs_cygwin.sh                                                                     000644  150261  000024  00000005750 12575563524 020144  0                                                                                                    ustar 00cedavid                         staff                           000000  000000                                                                                                                                                                         #!/bin/bash
#*******************************************************************************
#rapid_install_prereqs_cygwin.sh
#*******************************************************************************

#Purpose:
#This shell script installs programs required for RAPID. 
#Author:
#Alan D. Snow and Cedric H. David, 2015, based on tutorial by Cedric H. David


#*******************************************************************************
#Before installing 
#*******************************************************************************
#Make sure this file has execute privileges.  
# $ chmod u+x rapid_install_prereqs.sh 
#
#Compilers for C, C++ and FORTRAN are needed in order to install the libraries 
#used by RAPID. Here we use the GNU Compiler Collection. Make all necessary 
#compilers are installed by executing:
# $ which gcc
# $ which g++
# $ which gfortran

#*******************************************************************************
#Installation directory
#*******************************************************************************
INSTALL_DIR=$HOME/installz
#Update the location of the installation directory as you wish, but do not move 
#anything after running this script, or do so at your own risks!


#*******************************************************************************
#Main script
#*******************************************************************************
mkdir -p $INSTALL_DIR

#-------------------------------------------------------------------------------
#netCDF
#-------------------------------------------------------------------------------
cd $INSTALL_DIR
wget -nc "http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-3.6.3.tar.gz"
tar -xzf netcdf-3.6.3.tar.gz
mkdir -p netcdf-3.6.3-install
cd netcdf-3.6.3
./configure CC=gcc CXX=g++ FC=gfortran --prefix=$INSTALL_DIR/netcdf-3.6.3-install 
make check > check.log
make install > install.log

#-------------------------------------------------------------------------------
#PETSc
#-------------------------------------------------------------------------------
cd $INSTALL_DIR
wget -nc "http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-3.3-p7.tar.gz"
tar -xzf petsc-3.3-p7.tar.gz
cd petsc-3.3-p7
./configure PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx --download-f-blas-lapack=1 --download-mpich=1 --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --with-clanguage=cxx --with-debugging=0 --with-windows-graphics=0
make PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx all
make PETSC_DIR=$PWD PETSC_ARCH=linux-gcc-cxx test

#-------------------------------------------------------------------------------
#TAO
#-------------------------------------------------------------------------------
cd $INSTALL_DIR
wget -nc "http://www.mcs.anl.gov/research/projects/tao/download/tao-2.1-p2.tar.gz"
tar -xzf tao-2.1-p2.tar.gz
cd tao-2.1-p2
make TAO_DIR=$PWD PETSC_DIR=$INSTALL_DIR/petsc-3.3-p7 PETSC_ARCH=linux-gcc-cxx all > make.log
make TAO_DIR=$PWD PETSC_DIR=$INSTALL_DIR/petsc-3.3-p7 PETSC_ARCH=linux-gcc-cxx tao_testfortran > fortran.log
                        LICENSE                                                                                             000644  150261  000024  00000002711 12616535503 012631  0                                                                                                    ustar 00cedavid                         staff                           000000  000000                                                                                                                                                                         Copyright (c) 2015, Alan D. Snow and Cedric H. David

All rights reserved. 

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met: 
* Redistributions of source code must retain the above copyright notice, this 
  list of conditions and the following disclaimer. 
* Redistributions in binary form must reproduce the above copyright notice, this 
  list of conditions and the following disclaimer in the documentation and/or 
  other materials provided with the distribution.
* The name Cedric H. David may not be used to endorse or promote products 
  derived from this software without specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL CEDRIC H. DAVID BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       