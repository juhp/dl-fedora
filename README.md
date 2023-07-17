# dl-fedora

[![Hackage](https://img.shields.io/hackage/v/dl-fedora.svg)](https://hackage.haskell.org/package/dl-fedora)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/dl-fedora/badge/lts)](http://stackage.org/lts/package/dl-fedora)
[![Stackage Nightly](http://stackage.org/package/dl-fedora/badge/nightly)](http://stackage.org/nightly/package/dl-fedora)

A tool for downloading Fedora images.
By default it targets the Workstation edition of Fedora.

Usage examples:

`dl-fedora rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`dl-fedora 38 silverblue` : downloads the Fedora Silverblue iso

`dl-fedora respin kde` : downloads the latest KDE Live respin

`dl-fedora 37 server --arch aarch64` : will download the Server iso for armv8

`dl-fedora --run 38` : will download Fedora Workstation and boot the Live image with qemu-kvm.

`dl-fedora --local rawhide` : shows the current locally available image (as well as the latest one). With `--dryrun` it doesn't check for newest iso.

By default dl-fedora downloads to `~/Downloads/`
(correctly the XDG user "DOWNLOADS" directory),
but if you create an `iso` subdirectory (`~/Downloads/iso/`)
it will use that directory instead.

If the image is already found to be downloaded
it will not be re-downloaded of course.
Curl is used to do the downloading: partial downloads will continue.

A symlink to the latest iso is also created:
eg for rawhide it might be `"Fedora-Workstation-Live-x86_64-Rawhide-latest.iso"`.

It also tries to check the iso checksum and its gpg signature.

## Usage
```shellsession
$ dl-fedora --version
0.9.4
$ dl-fedora --help
Fedora iso downloader

Usage: dl-fedora [--version] [-g|--gpg-keys]
                 [(-C|--no-checksum) | (-c|--checksum)] [-n|--dry-run]
                 [-T|--no-http-timeout] [-l|--local] [-r|--run] [-R|--replace]
                 [(-d|--dl) | (-k|--koji) | (-m|--mirror URL)] [-a|--arch ARCH]
                 RELEASE [EDITION]
  Tool for downloading Fedora iso file images.
  RELEASE = release number, respin, rawhide, test (Beta), stage (RC), eln, c9s
  EDITION = {cloud,container,everything,server,workstation,silverblue,kinoite,
             budgie,cinnamon,i3,kde,lxde,lxqt,mate,soas,sway,
             xfce} [default: workstation]

  See <https://fedoraproject.org/wiki/Infrastructure/MirrorManager>
  and also <https://fedoramagazine.org/verify-fedora-iso-file>.

Available options:
  -h,--help                Show this help text
  --version                Show version
  -g,--gpg-keys            Import Fedora GPG keys for verifying checksum file
  -C,--no-checksum         Do not check checksum
  -c,--checksum            Do checksum even if already downloaded
  -n,--dry-run             Don't actually download anything
  -T,--no-http-timeout     Do not timeout for http response
  -l,--local               Show current local image via symlink
  -r,--run                 Boot image in Qemu
  -R,--replace             Delete previous snapshot image after downloading
                           latest one
  -d,--dl                  Use dl.fedoraproject.org
  -k,--koji                Use koji.fedoraproject.org
  -m,--mirror URL          Mirror url for /pub [default
                           https://download.fedoraproject.org/pub]
  -a,--arch ARCH           Architecture [default: x86_64]
```

## Contribution
dl-fedora is distributed under the GPL 3 license or later.

Please report issues or pull requests at <https://github.com/juhp/dl-fedora>.
