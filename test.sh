#!/bin/bash

DL_FEDORA="dist/build/dl-fedora/dl-fedora -n"

$DL_FEDORA 30
$DL_FEDORA rawhide -e silverblue
$DL_FEDORA devel -e container
$DL_FEDORA respin
$DL_FEDORA 29 -e kde
$DL_FEDORA 30 -e everything
