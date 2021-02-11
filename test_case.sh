#!/bin/bash

script=neg_3o2.F90

export OMP_NUM_THREADS=8
export FFLAGS="-O0 -g -pg -fbounds-check"
export FFLAGS="-O2 -fopenmp -ftree-vectorize -march=znver1 -mtune=generic"
export FFLAGS="-O2 -ftree-parallelize-loops=${OMP_NUM_THREADS} -ftree-vectorize -march=znver1 -mtune=generic"

if [ $# -gt 0 ]; then
  FFLAGS="${FFLAGS} -DONEoTWO"
fi
versions="A B C D E F G"

echo $FFLAGS
for version in ${versions}
do
  echo $version
  gfortran ${FFLAGS} -DTHREEHALVES_${version} ${script} && time ./a.out
  echo ""; echo "";
done
echo CHECK
gfortran ${FFLAGS} -DCHECK ${script} && ./a.out
