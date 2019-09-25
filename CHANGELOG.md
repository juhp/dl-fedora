# Changelog

## 0.7.1 (2019-09-25)
- print datestamp beside filesize
- show directory correctly with ~/
- use a subdirectory for checksum files

## 0.7 (2019-09-12)
- add --checksum to always do checksum when possible
- add --run to run image in qemu-kvm if available
- rework algorithms
  - check local filesize earlier: don't even check mirror if already downloaded
  - for partial local file output percentage already downloaded
  - otherwise show filesize
- show dir for symlink: so one knows location
- drop the 'devel' target (use release version number instead)
- handle old dangling symlink too (after deleting iso)

## 0.6 (2019-09-02)
- major rework to correct the url logic
  - first checks on dl.fedoraproject.org (master)
  - then tries to download corresponding filepath on download.fedoraproject.org
  - or falls back to master mirror
- new --dl option to download directly from dl.fedoraproject.org
- can now find branched development release
- also compares mirror and master filesizes
- builds with lts-14

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
