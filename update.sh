#! /bin/bash

#=========================================================================
# Copyright (C) GemStone Systems, Inc. 2010.
#
# Name - update.sh
#
# Purpose - Automatically update to a new version of GemStone
#           in an existing git repository cloned from MagLev on github.  
#           Be both verbose and idempotent, so we can easily diagnose
#           any problems.
#
# $Id:$
#
# Description:
#    Updates GemStone to a version corresponsing to $MAGLEV_HOME/version.txt
#    Safe to run multiple times. Only saves one prior backup repository though.
#
# Actions:
#    Download the GemStone archive into the parent directory
#    Uncompress the GemStone archive in the parent directory
#    Update the gemstone link to point to the new GemStone
#    Backup any existing 'maglev' repository
#    Wipeout any previous 'maglev' configuration file
#    Create a new default repository called 'maglev'
#    Generate the MagLev HTML documentation
#    Print build version information
#    Remind user to setup environment variables
#=========================================================================

if [ ! -d ".git" ]; then
    echo "[Error] $PWD is not a git repository"
    echo "install.sh and update.sh are only used with MagLev git repositories" 
    echo "for more information see http://github.com/MagLev/maglev"
    exit 1
fi

if [ -x bin/maglev-ruby ]; then
    # echo "using $PWD as MAGLEV_HOME"
    export MAGLEV_HOME=$PWD
else
    echo "[Error] $PWD is not a valid MagLev directory"
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

# We should run this as a normal user, not root.
if [ `id | cut -f2 -d= | cut -f1 -d\(` -eq 0 ]; then
    echo "[Error] This script should be run as a normal user, not root."
    exit 1
fi

# Detect operating system
PLATFORM="`uname -sm | tr ' ' '-'`"
if [ $PLATFORM = 'Darwin-x86_64' ]; then PLATFORM='Darwin-i386'; fi
gsvers=`grep ^GEMSTONE version.txt | cut -f2 -d-`
gss_name="GemStone-${gsvers}.${PLATFORM}"
gss_file=${gss_name}.tar.gz

# We're good to go. Let user know.
machine_name="`uname -n`"
echo "[Info] Starting upgrade to $gss_name on $machine_name"

# Look for either wget or curl to download GemStone
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
if [ ! -e $gss_file ]; then
    echo "[Info] Downloading $gss_file using ${cmd}"
    $cmd http://glass-downloads.gemstone.com/maglev/$gss_file
else
    echo "[Info] $gss_file already exists"
    echo "to replace it, remove or rename it and rerun this script"
fi

# Uncompress the downloaded GemStone archive in the current directory
echo "[Info] Uncompressing $gss_file in $PWD"
if [ ! -e $gss_name ]; then
    gunzip -c $gss_file | tar xf -
else
    echo "[Warning] $gss_name already exists"
    echo "to replace it, remove or rename it and rerun this script"
fi

# Create a link to the GemStone directory
echo "[Info] Linking $gss_name to ${MAGLEV_HOME}/gemstone"
rm -f $MAGLEV_HOME/gemstone
ln -sf ${PWD}/$gss_name $MAGLEV_HOME/gemstone

# Finally get back to the MagLev directory
cd $MAGLEV_HOME

# Make sure we have a locks directory
mkdir -p locks
# and the correct updated keyfile
rm -f etc/maglev.demo.key
ln -sf maglev.demo.key-$PLATFORM etc/maglev.demo.key
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

echo "[Info] Finished upgrade to $gss_name on $machine_name"
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
echo "[Info] If you want to call GemStone Smalltalk methods from Ruby, run"
echo "  rake stwrappers"
echo "after this upgrade has finished. This will generate .rb files you can use"
echo "in \$MAGLEV_HOME/lib/ruby/site_ruby/1.8/smalltalk/"

# End of script
exit 0
