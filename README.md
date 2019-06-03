# fedora-img-dl

[![Hackage](https://img.shields.io/hackage/v/fedora-img-dl.svg)](https://hackage.haskell.org/package/fedora-img-dl)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/fedora-img-dl/badge/lts)](http://stackage.org/lts/package/fedora-img-dl)
[![Stackage Nightly](http://stackage.org/package/fedora-img-dl/badge/nightly)](http://stackage.org/nightly/package/fedora-img-dl)

A tool for downloading Fedora images.
By default it targets the Workstation edition of Fedora.

Usage example:

`fedora-img-dl rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`fedora-img-dl -e silverblue 30` : downloads Fedora 30 Silverblue iso

`fedora-img-dl -e kde respin` : downloads the latest KDE Live respin

`fedora-img-dl --edition server --arch aarch64 29` : will bring down the F29 Server iso

A symlink to the latest iso is also created:
eg for rawhide it might be `"Fedora-Workstation-Live-x86_64-Rawhide-latest.iso"`.

When available it also tries to check the iso checksum and its gpg signature.
