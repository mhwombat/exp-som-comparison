#!/bin/sh
label=Big
/home/eamybut/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-quick-sos > scs${label}Quick.html
#/home/eamybut/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-quick-som > som${label}Quick.html
time /home/eamybut/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-sos > scs${label}.log 2>&1
#time /home/eamybut/nosync/sandboxes/exp-som-comparison/bin/exp-som-comparison-som > som${label}.log 2>&1
grep '<img' scs${label}.log > scs${label}.html
# grep '<img' som${label}.log > som${label}.html

