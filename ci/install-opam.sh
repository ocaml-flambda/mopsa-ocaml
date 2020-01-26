#!/bin/sh

# Non-interactive opam installer
# Inspired from the official install script

VERSION='2.0.6'
TAG=$(echo "$VERSION" | tr '~' '-')

ARCH=$(uname -m || echo unknown)
case "$ARCH" in
    x86|i?86) ARCH="i686";;
    x86_64|amd64) ARCH="x86_64";;
    ppc|powerpc|ppcle) ARCH="ppc";;
    aarch64_be|aarch64|armv8b|armv8l) ARCH="arm64";;
    armv5*|armv6*|earmv6*|armv7*|earmv7*) ARCH="armhf";;
    *) ARCH=$(echo "$ARCH" | awk '{print tolower($0)}')
esac

OS=$( (uname -s || echo unknown) | awk '{print tolower($0)}')

if [ "$OS" = "darwin" ] ; then
  OS=macos
fi

OPAM_BIN_URL_BASE='https://github.com/ocaml/opam/releases/download/'
OPAM_BIN="opam-${TAG}-${ARCH}-${OS}"
OPAM_BIN_URL="${OPAM_BIN_URL_BASE}${TAG}/${OPAM_BIN}"

mkdir /tmp/opam
wget -O /tmp/opam/opam.bin ${OPAM_BIN_URL}
install -m 755 /tmp/opam/opam.bin /usr/bin/opam
