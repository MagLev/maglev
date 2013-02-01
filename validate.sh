#! /bin/bash

#=========================================================================
# Copyright (C) GemStone Systems, Inc. 2010.
#
# Name - install.sh
#
# Purpose - Make sure system can run MagLev.
#
# $Id:$
#
# Description:
#    Verify machine is capable of running MagLev 64-bit
#
# Actions:
#    Verify machine is capable of running MagLev 64-bit
#=========================================================================

# Detect operating system
PLATFORM="`uname -sm | tr ' ' '-'`"
# Macs with Core i7 use the same software as older Macs
[[ $PLATFORM == "Darwin-x86_64" ]] && PLATFORM="Darwin-i386"

# Check we're on a suitable 64-bit machine
case "$PLATFORM" in
  Darwin-i386)
    OSVERSION="`sw_vers -productVersion`"
    MAJOR="`echo $OSVERSION | cut -f1 -d.`"
    MINOR="`echo $OSVERSION | cut -f2 -d.`"
    CPU_TYPE="`uname -p`"
    CPU_CAPABLE="`sysctl hw.cpu64bit_capable | cut -f2 -d' '`"
    #
    # Check the CPU and Mac OS X profile.
    if [[ $CPU_TYPE != "i386" || $CPU_CAPABLE -ne 1 || $MAJOR -lt 10 || $MINOR -lt 5 ]] ; then
        echo "[Error] This script requires Mac OS X 10.5 or later on a 64-bit Intel CPU."
        exit 1
    fi
    ;;
  Linux-x86_64)
    # Linux looks OK
    ;;
  SunOS-i86pc)
    # Solaris X86  looks OK
    ;;
  *)
    echo "[Error] This script only works on a 64-bit Linux, Mac OS X, or Solaris-x86 machine"
    echo "The result from \"uname -sm\" is \"`uname -sm`\""
    exit 1
    ;;
esac
