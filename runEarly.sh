#!/bin/bash

testDir=$HOME/mnist/testData

function runit {
  echo $*
  time (~/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-sgm \
    $trainingDir $testDir $1 $2 $3) >sgm-$1-$2-$3-just$7.log 2>&1
  time (~/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-som \
    $trainingDir $testDir $4 $2 $3 $5 $6) >som-$4-$2-$3-$5-$6-just$7.log 2>&1
}


# arg     1      2  3 4  5  6                 7
#     threshold r0 rf s w0 wf numTrainingImages

# n=500
# trainingDir=$HOME/mnist/random${n}trainingData
# runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

n=400
trainingDir=$HOME/mnist/random${n}trainingData
runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

n=300
trainingDir=$HOME/mnist/random${n}trainingData
runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

# n=250
# trainingDir=$HOME/mnist/random${n}trainingData
# runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

n=200
trainingDir=$HOME/mnist/random${n}trainingData
runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

n=150
trainingDir=$HOME/mnist/random${n}trainingData
runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

# n=100
# trainingDir=$HOME/mnist/random${n}trainingData
# runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

n=75
trainingDir=$HOME/mnist/random${n}trainingData
runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

# n=50
# trainingDir=$HOME/mnist/random${n}trainingData
# runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

# n=25
# trainingDir=$HOME/mnist/random${n}trainingData
# runit 0.10 0.1 0.0001 32 2 0.0001 ${n}

