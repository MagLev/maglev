#! /bin/bash

#=========================================================================
# Copyright (C) GemStone Systems, Inc. 2009.
#
# Name - update.sh
#
# Purpose - Automatically update to a new version of MagLev and GemStone
#           in an existing git repository cloned from MagLev on github.  
#           Be both verbose and idempotent, so we can easily diagnose
#           any problems.
#
# $Id:$
#
# Description:
#    Does a basic update of MagLev
#    Setup for manual MagLev startup rather than automatic startup upon boot
#    Safe to run multiple times. Only saves one prior backup repository though.
#
# Actions:
#    Download the GemStone product zipfile into the parent directory
#    Uncompress the GemStone zipfile into the parent directory
#    Update the gemstone link to point to the new GemStone
#    Backup any existing 'maglev' repository
#    Wipeout any previous 'maglev' configuration file
#    Create a new default repository called 'maglev'
#    Generate the MagLev HTML documentation
#    Print build version information
#    Remind user to setup environment variables
#=========================================================================

if [ -x bin/maglev-ruby ]; then
  # echo "using $PWD as MAGLEV_HOME"
  export MAGLEV_HOME=$PWD
else
  echo "[Error] $PWD is not a valid maglev git repository"
  echo "To fix this, 'clone git://github.com:MagLev/maglev.git'"
  echo "then run install.sh from the resulting directory."
  exit 1
fi

# Check that the parent directory is writable
if [ ! -w ".." ]; then
    echo "[Error] This script requires write permission on the MagLev parent directory."
    /bin/ls -ld ..
    echo "To fix this, 'chmod u+w ..'"
    exit 1
fi

# Detect operating system
OS=`uname -s`
[ $OS = "Darwin" ] && OS="MacOSX"
[ $OS = "SunOS" ] && [ "`uname -m`" = "i86pc" ] && OS="Solaris-x86"

gsvers=`grep ^GEMSTONE version.txt | cut -f2 -d-`
mlvers="MagLev-${gsvers}.${OS}"
gsname="GemStone-${gsvers}.${OS}"
zipfile=${gsname}.zip

# We should run this as a normal user, not root.
if [ `id | cut -f2 -d= | cut -f1 -d\(` -eq 0 ]; then
    echo "[Error] This script should be run as a normal user, not root."
    exit 1
fi

# We're good to go. Let user know.
machine_name="`uname -n`"
echo "[Info] Starting installation of $mlvers on $machine_name"

# Look for either wget or curl to download MagLev
if [ -e "`which wget`" ]; then
    cmd="`which wget`"
elif [ -e "`which curl`" ]; then
    cmd="`which curl` -O"
else
    echo "[Error] Neither wget nor curl is available. Install one of them and rerun this script."
    exit 1
fi

# IMPORTANT: Move to the parent directory of the MagLev git repository
cd $MAGLEV_HOME/..

# Download appropriate version of GemStone
if [ ! -e $zipfile ]; then
    echo "[Info] Downloading GemStone archive using ${cmd}"
    $cmd http://glass-downloads.gemstone.com/maglev/$zipfile
else
    echo "[Info] $zipfile already exists"
    echo "to replace it, remove or rename it and rerun this script"
fi

# Unzip the downloaded archive into the current directory
echo "[Info] Uncompressing GemStone archive into $PWD"
if [ ! -e $gsname ]; then
    unzip -q $zipfile
else
    echo "[Warning] $gsname already exists"
    echo "to replace it, remove or rename it and rerun this script"
fi

echo "[Info] Linking gemstone to ${PWD}/$gsname"
rm -f $MAGLEV_HOME/gemstone
ln -s ${PWD}/$gsname $MAGLEV_HOME/gemstone

# Finally get back to the MagLev directory
cd $MAGLEV_HOME

# Make sure we have a locks directory
mkdir -p locks
# and the correct updated keyfile
ln -sf maglev.demo.key-$OS etc/maglev.demo.key
# Make sure we have specs and benchmarks.
echo "[Info] updating MSpec, RubySpec, and RBS submodules"
git submodule update --init 

# Create a default repository called "maglev" and generate the MagLev HTML documentation
# Check for existence of required executable /usr/bin/rake
if [ -x /usr/bin/rake ]; then
    # Backup any existing maglev repository
    if [ -e data/maglev/extent/extent0.ruby.dbf ]; then 
        echo "[Info] Backing up existing 'maglev' repository to backups/previous_maglev_extent.tgz"
        rake maglev:take_snapshot >/dev/null
        mv backups/maglev_extent.tgz backups/previous_maglev_extent.tgz
    fi
    # create a clean slate
    if [ -e etc/conf.d/maglev.conf ]; then
        echo "[Info] Removing existing 'maglev' configuration file."
        rake stone:destroy[maglev] >/dev/null
    fi
    echo "[Info] Creating new default 'maglev' repository"
    rake stone:create[maglev] >/dev/null
    echo "[Info] Generating the MagLev HTML documentation"
    rake rdoc >/dev/null 2>&1
else
    echo "[Warning] /usr/bin/rake not found!"
    echo "Skipping creation of default 'maglev' repository and HTML documentation."
fi

echo "[Info] Finished upgrade to $mlvers on $machine_name"
echo ""
echo "[Info] MagLev version information:"
cat version.txt
echo "[Info] GemStone version information:"
cat gemstone/version.txt

# Reminder to setup environment variables
echo ""
echo "[Info] Adding these to your .bashrc will make it easier to run MagLev"
echo "export MAGLEV_HOME=${PWD}"
echo 'export PATH=$MAGLEV_HOME/bin:$PATH'

# Reminder to generate Smalltalk FFI
echo ""
echo "[Info] After you complete this upgrade and verify MagLev is working, run"
echo "  rake stwrappers"
echo "to generate the .rb files for the GemStone/Smalltalk FFI"
echo "in MAGLEV_HOME/lib/ruby/site_ruby/1.8/smalltalk/"

# End of script
exit 0
