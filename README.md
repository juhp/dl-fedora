# dl-fedora

[![Hackage](https://img.shields.io/hackage/v/dl-fedora.svg)](https://hackage.haskell.org/package/dl-fedora)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/dl-fedora/badge/lts)](http://stackage.org/lts/package/dl-fedora)
[![Stackage Nightly](http://stackage.org/package/dl-fedora/badge/nightly)](http://stackage.org/nightly/package/dl-fedora)

A tool for downloading Fedora images.
By default it targets the Workstation edition of Fedora.

Usage examples:

`dl-fedora rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`dl-fedora 35 silverblue` : downloads the Fedora Silverblue iso

`dl-fedora respin kde` : downloads the latest KDE Live respin

`dl-fedora 34 server --arch aarch64` : will bring down the Server iso for armv8

`dl-fedora --run 35` : will download Fedora Workstation and boot the Live image with qemu-kvm.

`dl-fedora --local rawhide` : shows the current locally available image (as well as the latest one). With `--dryrun` it doesn't check for newest iso.

By default dl-fedora downloads to `~/Downloads/`
(correctly the XDG user "DOWNLOADS" directory),
but if you create `~/Downloads/iso/` it will use that directory instead.

If the image is already found to be downloaded
it will not be re-downloaded of course.
Curl is used to do the downloading: partial downloads will continue.

A symlink to the latest iso is also created:
eg for rawhide it might be `"Fedora-Workstation-Live-x86_64-Rawhide-latest.iso"`.

It also tries to check the iso checksum and its gpg signature.
