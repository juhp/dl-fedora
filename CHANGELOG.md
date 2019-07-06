# Changelog

## 0.5 (2019-07-06)
- no hardcoding of devel branch, beta, and respins
- fix Spins and Cloud/Container paths
- use rawhide for devel if not branched
- don't checksum if no file downloaded or new --no-checksum
- more informative error messages
- add simple dryrun test script

## 0.4 (2019-06-03)
- drop version 30 special case
- support gpg verification of checksum file

## 0.3 (2019-04-16)
- run sha256sum check
- support fedora Spins

## 0.2
- fix and improve symlink naming
- use new http-directory library to check exact filesize

## 0.1
* initial release
