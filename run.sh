#!/bin/bash

trainingDir=$HOME/mnist/trainingData
testDir=$HOME/mnist/testData
#trainingDir=$HOME/mnist/smallSet
#testDir=$HOME/mnist/smallSet

function runit {
  echo $*
  time (~/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-sgm \
    $trainingDir $testDir $1 $2 $3) >sgm-$1-$2-$3.log 2>&1
  time (~/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-som \
    $trainingDir $testDir $4 $2 $3 $5 $6) >som-$4-$2-$3-$5-$6.log 2>&1
}

# arg     1      2  3 4  5  6
#     threshold r0 rf s w0 wf

runit 0.10 0.1 0.0001 32 2 0.0001
runit 0.165 0.1 0.0001 10 2 0.0001
