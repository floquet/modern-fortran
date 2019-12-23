#!/bin/bash
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
