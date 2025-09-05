#!/bin/bash

# Builds ALPS on Rusty
# 1. Sets the module environment
# 2. Defines necessary environment variables
# 3. Runs necessary build commands

# Configure automake
autoreconf -i -f

# Load appropriate modules and create symbolic links for openblas
# Delete existing lib/ and create new to accomodate the openblas version loaded
module --force purge
ml modules/2.3-beta2 slurm gcc/11.4.0 openmpi/4.0.7 openblas/threaded-0.3.26 hdf5/mpi-1.12.3 python/3.10.13
#module --force purge
#ml modules/2.3-beta2 slurm intel-oneapi-compilers/2024.1.0 intel-oneapi-mpi/intel-2021.12.0  openblas/threaded-0.3.26 hdf5/intel-mpi-1.14.3 python/3.10.13
rm -r lib/
mkdir lib
cd lib
ln -s /mnt/sw/nix/store/c46gznhkrwf31qhghwhfcrxhfh87vzi8-openblas-0.3.26/lib/libopenblas.so libblas.so
ln -s /mnt/sw/nix/store/c46gznhkrwf31qhghwhfcrxhfh87vzi8-openblas-0.3.26/lib/libopenblas.so liblapack.so
# Set linker flags
export LDFLAGS=-L/mnt/home/vdhruv/ALPS/lib
export FCFLAG=S-L/mnt/home/vdhruv/ALPS/lib

# Print module set and environment variables
module show
echo $LDFLAGS
echo $FCFLAGS

# Configure and build ALPS
cd ../
./configure
make clean
make
