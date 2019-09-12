#!/bin/bash

DL_FEDORA="dist/build/dl-fedora/dl-fedora -n"

function runtest {
    echo ">> $DL_FEDORA $*"
    $DL_FEDORA $*
    echo
}

runtest 30
runtest rawhide -e silverblue
runtest 31 -e container
runtest respin
runtest 29 -e kde
runtest 30 -e everything
runtest 30 -e server --arch aarch64
runtest 31
