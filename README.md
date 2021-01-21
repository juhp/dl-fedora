# dl-fedora

[![GitHub CI](https://github.com/juhp/dl-fedora/workflows/build/badge.svg)](https://github.com/juhp/dl-fedora/actions)
[![Hackage](https://img.shields.io/hackage/v/dl-fedora.svg)](https://hackage.haskell.org/package/dl-fedora)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/dl-fedora/badge/lts)](http://stackage.org/lts/package/dl-fedora)
[![Stackage Nightly](http://stackage.org/package/dl-fedora/badge/nightly)](http://stackage.org/nightly/package/dl-fedora)

A tool for downloading Fedora images.
By default it targets the Workstation edition of Fedora.

Usage examples:

`dl-fedora rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`dl-fedora -e silverblue 33` : downloads the Fedora Silverblue iso

`dl-fedora -e kde respin` : downloads the latest KDE Live respin

`dl-fedora --edition server --arch aarch64 32` : will bring down the F32 Server iso

`dl-fedora --run 33` : will download Fedora 33 Workstation and boot the Live image with qemu-kvm.

If the image is already in the Downloads/ directory
it will not be downloaded again of course.
Curl is used to do the downloading.

A symlink to the latest iso is also created:
eg for rawhide it might be `"Fedora-Workstation-Live-x86_64-Rawhide-latest.iso"`.

It also tries to check the iso checksum and its gpg signature.
