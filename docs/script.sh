#!/bin/bash

# List of values
for G in 11 12 13 14 15 16 17 21 22 23 24 25 26 27 28 29 31 32 33 35 41 42 43 50 51 52 53; do
    # Create a new file by copying the original and changing line 27
    sed "27s/.*/inputSelectLoc2 <- $G/" dengue-report-base.Rmd > "reports/dengue-report-$G.Rmd"
done

