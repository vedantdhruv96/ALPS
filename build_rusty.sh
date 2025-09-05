#!/bin/bash

# Builds ALPS on Rusty
# 1. Sets the module environment
# 2. Defines necessary environment variables
# 3. Runs necessary build commands

# Configure automake
autoreconf -i -f

# Load appropriate modules and create symbolic links for openblas
# Delete existing lib/ and create new to accomodate the openblas version loaded
# module --force purge
module load openmpi4 openblas
rm -r lib/
mkdir lib
cd lib
ln -s /mnt/sw/nix/store/kkhxdzyg5kaxrxrl6j7pwapq7v7xacc7-openblas-0.3.26/lib/libopenblas.so.0 libblas.so
ln -s /mnt/sw/nix/store/kkhxdzyg5kaxrxrl6j7pwapq7v7xacc7-openblas-0.3.26/lib/libopenblas.so.0 liblapack.so
# Set linker flags
export LDFLAGS=-L/mnt/home/vdhruv/ALPS/lib
export FCFLAGS=-L/mnt/home/vdhruv/ALPS/lib

# Print module set and environment variables
module list
echo $LDFLAGS
echo $FCFLAGS

# Configure and build ALPS
cd ../
./configure
make clean
make
