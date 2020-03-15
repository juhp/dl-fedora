# dl-fedora

Earlier called `fedora-img-dl`.

[![Hackage](https://img.shields.io/hackage/v/dl-fedora.svg)](https://hackage.haskell.org/package/dl-fedora)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/dl-fedora/badge/lts)](http://stackage.org/lts/package/dl-fedora)
[![Stackage Nightly](http://stackage.org/package/dl-fedora/badge/nightly)](http://stackage.org/nightly/package/dl-fedora)

A tool for downloading Fedora images.
By default it targets the Workstation edition of Fedora.

Usage example:

`dl-fedora rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`dl-fedora -e silverblue 31` : downloads Fedora 30 Silverblue iso

`dl-fedora -e kde respin` : downloads the latest KDE Live respin

`dl-fedora --edition server --arch aarch64 30` : will bring down the F29 Server iso

`dl-fedora --run 32` : will download Fedora 32 Workstation and boot the Live image with qemu-kvm.

A symlink to the latest iso is also created:
eg for rawhide it might be `"Fedora-Workstation-Live-x86_64-Rawhide-latest.iso"`.

When available it also tries to check the iso checksum and its gpg signature.
