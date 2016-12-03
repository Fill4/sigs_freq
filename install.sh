#!/bin/bash

cd lib/
make
cd ../src/
make
mv sigs_freq ../sigs_freq
cd ../
