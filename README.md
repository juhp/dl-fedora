# dl-fedora

[![Hackage](https://img.shields.io/hackage/v/dl-fedora.svg)](https://hackage.haskell.org/package/dl-fedora)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/dl-fedora/badge/lts)](http://stackage.org/lts/package/dl-fedora)
[![Stackage Nightly](http://stackage.org/package/dl-fedora/badge/nightly)](http://stackage.org/nightly/package/dl-fedora)

A tool for downloading Fedora, ELN, and Centos Stream images.
By default it targets the Workstation edition of Fedora.

Usage examples:

`dl-fedora rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`dl-fedora 39 silverblue` : downloads the Fedora Silverblue iso

`dl-fedora respin kde` : downloads the latest KDE Live respin

`dl-fedora 38 server --arch aarch64` : will download the Server iso for armv8

`dl-fedora --run 39` : will download Fedora Workstation and boot the Live image with qemu-kvm.

`dl-fedora --check 39 respin` : checks if there is a newer 39 respin iso image
available.

`dl-fedora --local rawhide` : shows the current locally available image
(as well as the latest one available, unless `--dry-run` is given).
This can be combined with `--run` to quickly run the latest local image,
without a newer download.

`dl-fedora c9s` : downloads a Centos Stream 9 net installer.

By default dl-fedora downloads to `~/Downloads/`
(correctly the XDG user "DOWNLOADS" directory),
but if you create an `iso` subdirectory there (`~/Downloads/iso/`)
it will use that directory instead.

`dl-fedora` downloads the latest mirrored image redirected from
`download.fedoraproject.org` by default.
If you want to ensure getting the very latest image you can use `--latest`,
which will then download from `dl.fedoraproject.org` instead
_if_ your mirror is not synced yet. (This behavior changed with 0.10.)

If the image is already found to be downloaded
it will not be re-downloaded of course.
Curl is used to do the downloading: partial downloads will continue.

A symlink to the latest iso is also created:
eg for rawhide it might be `"Fedora-Workstation-Live-x86_64-Rawhide-latest.iso"`.

It also tries to check the iso checksum and its gpg signature.

## Usage
```shellsession
$ dl-fedora --version
0.10
$ dl-fedora --help
Fedora iso downloader

Usage: dl-fedora [--version] [-g|--gpg-keys] [--no-checksum | --checksum]
                 [--debug] [-T|--no-http-timeout]
                 [(-c|--check) | [-l|--local] [-n|--dry-run] [-r|--run]]
                 [-R|--replace]
                 [(-L|--latest) | (-d|--dl) | (-k|--koji) | (-m|--mirror URL)]
                 [--cs-devel | --cs-test] [-a|--arch ARCH] RELEASE [EDITION]

  Tool for downloading Fedora iso file images.
  RELEASE = release number, respin, rawhide, test (Beta), stage (RC), eln, c8s, c9s
  EDITION = {cloud,container,everything,server,workstation,budgie,cinnamon,i3,
             kde,lxde,lxqt,mate,soas,sway,xfce,silverblue,kinoite,onyx,
             sericea} [default: workstation]

  See <https://fedoraproject.org/wiki/Infrastructure/MirrorManager>
  and also <https://fedoramagazine.org/verify-fedora-iso-file>.

Available options:
  -h,--help                Show this help text
  --version                Show version
  -g,--gpg-keys            Import Fedora GPG keys for verifying checksum file
  --no-checksum            Do not check checksum
  --checksum               Do checksum even if already downloaded
  --debug                  Debug output
  -T,--no-http-timeout     Do not timeout for http response
  -c,--check               check if newer image available
  -l,--local               Show current local image via symlink
  -n,--dry-run             Don't actually download anything
  -r,--run                 Boot image in Qemu
  -R,--replace             Delete previous snapshot image after downloading
                           latest one
  -L,--latest              Get latest image either from mirror or dl.fp.o if
                           newer
  -d,--dl                  Use dl.fedoraproject.org (dl.fp.o)
  -k,--koji                Use koji.fedoraproject.org
  -m,--mirror URL          Mirror url for /pub [default
                           https://download.fedoraproject.org/pub]
  --cs-devel               Use centos-stream development compose
  --cs-test                Use centos-stream test compose (default is
                           production)
  -a,--arch ARCH           Specify arch [default: x86_64]
```

## Contribution
dl-fedora is distributed under the GPL license version 3 or later.

Please report issues or pull requests at <https://github.com/juhp/dl-fedora>.
