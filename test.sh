#!/bin/bash

DL_FEDORA="dist/build/dl-fedora/dl-fedora"

function runtest {
    echo ">> $DL_FEDORA $*"
    $DL_FEDORA $*
    echo
}

runtest -n 31 -c
runtest -n rawhide -e silverblue
runtest -n 32 -e container
runtest respin
runtest -n 30 -e kde
runtest -n 31 -e everything
runtest -n 31 -e server --arch aarch64
runtest -n 32
