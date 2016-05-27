#!/bin/bash

trainingDir=$HOME/mnist/trainingData
testDir=$HOME/mnist/testData
#trainingDir=$HOME/mnist/smallSet
#testDir=$HOME/mnist/smallSet

function runit {
  echo $*
#  time (~/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-sgm \
#    $trainingDir $testDir $1 $2 $3) >sgm-$1-$2-$3.log 2>&1
  time (~/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-som \
    $trainingDir $testDir $4 $2 $3 $5 $6) >som-$4-$2-$3-$5-$6.log 2>&1
}

# arg     1      2  3 4  5  6
#     threshold r0 rf s w0 wf

#runit 0.10 0.1 0.0001 55 2 0.0001
runit 0.11 0.1 0.0001 41 2 0.0001
runit 0.115 0.1 0.0001 36 2 0.0001
runit 0.116 0.1 0.0001 35 2 0.0001
runit 0.117 0.1 0.0001 34 2 0.0001
runit 0.118 0.1 0.0001 33 2 0.0001
runit 0.119 0.1 0.0001 32 2 0.0001
runit 0.12 0.1 0.0001 31 2 0.0001
runit 0.13 0.1 0.0001 24 2 0.0001
runit 0.14 0.1 0.0001 18 2 0.0001
runit 0.15 0.1 0.0001 14 2 0.0001
runit 0.16 0.1 0.0001 11 2 0.0001
runit 0.17 0.1 0.0001 9 2 0.0001
runit 0.18 0.1 0.0001 7 2 0.0001
runit 0.19 0.1 0.0001 6 2 0.0001
runit 0.2 0.1 0.0001 5 2 0.0001

runit 0.10 0.1 0.0001 55 2 0.0001

#runit 0.11 0.1 0.0001 32 2 0.0001
#runit 0.115 0.1 0.0001 32 2 0.0001
#runit 0.116 0.1 0.0001 32 2 0.0001
#runit 0.117 0.1 0.0001 32 2 0.0001
#runit 0.118 0.1 0.0001 32 2 0.0001
#runit 0.119 0.1 0.0001 32 2 0.0001
#runit 0.12 0.1 0.0001 32 2 0.0001
#runit 0.165 0.1 0.0001 10 2 0.0001
