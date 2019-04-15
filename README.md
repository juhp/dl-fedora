# fedora-img-dl

[![Hackage](https://img.shields.io/hackage/v/fedora-img-dl.svg)](https://hackage.haskell.org/package/fedora-img-dl)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/fedora-img-dl/badge/lts)](http://stackage.org/lts/package/fedora-img-dl)
[![Stackage Nightly](http://stackage.org/package/fedora-img-dl/badge/nightly)](http://stackage.org/nightly/package/fedora-img-dl)

A tool for downloading Fedora images.
By default it targets the Workstation edition of Fedora.

Usage example:

`fedora-img-dl rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`fedora-img-dl respin` : downloads the latest Live Workstation respin

`fedora-img-dl -e silverblue 30` : downloads Fedora 30 Silverblue iso

`fedora-img-dl --edition server --arch aarch64 29` : will bring down the F29 Server iso

(Currently Spins are not yet supported.)

A symlink to the latest iso is also created:
eg for rawhide it might be `Fedora-Workstation-Live-x86_64-Rawhide-latest.iso`.
