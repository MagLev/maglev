#! /bin/bash

#=========================================================================
# Copyright (C) GemStone Systems, Inc. 2009.
#
# Name - install.sh
#
# Purpose - Automatically download and install a version of MagLev
#           into an existing git repository cloned from MagLev on github.  
#           Be both verbose and idempotent, so we can easily diagnose
#           any problems.
#
# $Id:$
#
# Description:
#    Does a basic installation of MagLev
#    Setup for manual MagLev startup rather than automatic startup upon boot
#    Safe to run multiple times. Only saves one prior backup repository though.
#    Requires root access (using sudo) to change setings and create directories
#
# Actions:
#    Verify machine is capable of running MagLev 64-bit
#    Add shared memory setup to /etc/sysctl.conf
#    Add GemStone netldi service port to /etc/services
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

# Check we're on a suitable 64-bit machine
case "$OS" in
    Linux)
    if [ "`uname -sm`" != "Linux x86_64" ]; then
        echo "[Error] This script only works on a 64-bit Linux OS."
        echo "The result from \"uname -sm\" is \"`uname -sm`\" not \"Linux x86_64\""
        exit 1
    fi
    ;;
    MacOSX)
    OSVERSION="`sw_vers -productVersion`"
    MAJOR="`echo $OSVERSION | cut -f1 -d.`"
    MINOR="`echo $OSVERSION | cut -f2 -d.`"
    CPU_TYPE="`uname -p`"
    CPU_CAPABLE="`sysctl hw.cpu64bit_capable | cut -f2 -d' '`"
    #
    # Check the CPU and Mac OS profile.
    if [[ $CPU_TYPE != "i386" || $CPU_CAPABLE -ne 1 || $MAJOR -lt 10 || $MINOR -lt 5 ]] ; then
        echo "[Error] This script requires Mac OS 10.5 or later on a 64-bit Intel CPU."
        exit 1
    fi
    ;;
    Solaris-x86)
    if [ "`uname -sm`" != "SunOS i86pc" ]; then 
        echo "[Error] This script only works on a 64-bit Solaris-x86 OS."
        echo "The result from \"uname -sm\" is \"`uname -sm`\" not \"SunOS i86pc\""
        exit 1
    fi
    ;;
    *)
    echo "[Error] This script only works on a 64-bit Linux, Mac OS X, or Solaris-x86 machine"
    echo "The result from \"uname -sm\" is \"`uname -sm`\""
    exit 1
    ;;
esac

# We should run this as a normal user, not root.
if [ `id | cut -f2 -d= | cut -f1 -d\(` -eq 0 ]; then
    echo "[Error] This script should be run as a normal user, not root."
    exit 1
fi

# We're good to go. Let user know.
machine_name="`uname -n`"
echo "[Info] Starting installation of $mlvers on $machine_name"

# Do a trivial sudo to test we can and get the password prompt out of the way
sudo date

# Figure out how much total memory is installed
echo "[Info] Setting up shared memory"
#
# Ref: http://wiki.finkproject.org/index.php/Shared_Memory_Regions_on_Darwin
# Ref: http://developer.postgresql.org/pgdocs/postgres/kernel-resources.html
# Ref: http://www.idevelopment.info/data/Oracle/DBA_tips/Linux/LINUX_8.shtml
#
case "$OS" in
    Linux)
    # use TotalMem: kB because Ubuntu doesn't have Mem: in Bytes
    totalMemKB=`awk '/MemTotal:/{print($2);}' /proc/meminfo`
    totalMem=$(($totalMemKB * 1024))
    # Figure out the max shared memory segment size currently allowed
    shmmax=`cat /proc/sys/kernel/shmmax`
    # Figure out the max shared memory currently allowed
    shmall=`cat /proc/sys/kernel/shmall`
    ;;
    MacOSX)
    totalMem="`sysctl hw.memsize | cut -f2 -d' '`"
    # Figure out the max shared memory segment size currently allowed
    shmmax="`sysctl kern.sysv.shmmax | cut -f2 -d' '`"
    # Figure out the max shared memory currently allowed
    shmall="`sysctl kern.sysv.shmall | cut -f2 -d' '`"
    ;;
    Solaris-x86)
    # TODO: figure memory needs for Solaris-x86
    # Investigate project.max-shm-memory
    totalMemMB="`/usr/sbin/prtconf | grep Memory | cut -f3 -d' '`"
    totalMem=$(($totalMemMB * 1048576))
    shmmax=$(($totalMem / 4))
    shmall=$(($shmmax / 4096))
    ;;
    *)
    echo "[Error] Can't determine operating system. Check script."
    exit 1
    ;;
esac
totalMemMB=$(($totalMem / 1048576))
shmmaxMB=$(($shmmax / 1048576))
shmallMB=$(($shmall / 256))

# Print current values
echo "  Total memory available is $totalMemMB MB"
echo "  Max shared memory segment size is $shmmaxMB MB"
echo "  Max shared memory allowed is $shmallMB MB"

# Figure out the max shared memory segment size (shmmax) we want
# Use 75% of available memory but not more than 2GB
shmmaxNew=$(($totalMem * 3/4))
[ $shmmaxNew -gt 2147483648 ] && shmmaxNew=2147483648
shmmaxNewMB=$(($shmmaxNew / 1048576))

# Figure out the max shared memory allowed (shmall) we want
# The MacOSX default is 4MB, way too small
# The Linux default is 2097152 or 8GB, so we should never need this
# but things will certainly break if it's been reset too small
# so ensure it's at least big enough to hold a fullsize shared memory segment
shmallNew=$(($shmmaxNew / 4096))
[ $shmallNew -lt $shmall ] && shmallNew=$shmall
shmallNewMB=$(($shmallNew / 256))

# Increase shmmax if appropriate
if [ $shmmaxNew -gt $shmmax ]; then
    echo "[Info] Increasing max shared memory segment size to $shmmaxNewMB MB"
    [ $OS = "MacOSX" ] && sudo sysctl -w kern.sysv.shmmax=$shmmaxNew
    [ $OS = "Linux" ] && sudo bash -c "echo $shmmaxNew > /proc/sys/kernel/shmmax"
    [ $OS = "Solaris-x86" ] && echo "[Warning] shmmax must be set manually on Solaris-x86"
else
    echo "[Info] No need to increase max shared memory segment size"
fi

# Increase shmall if appropriate
if [ $shmallNew -gt $shmall ]; then
    echo "[Info] Increasing max shared memory allowed to $shmallNewMB MB"
    [ $OS = "MacOSX" ] && sudo sysctl -w kern.sysv.shmall=$shmallNew
    [ $OS = "Linux" ] && sudo bash -c "echo $shmallNew > /proc/sys/kernel/shmall"
    [ $OS = "Solaris-x86" ] && echo "[Warning]shmall must be set manually on Solaris-x86"
else
    echo "[Info] No need to increase max shared memory allowed"
fi

# At this point, shared memory settings contain the values we want, 
# put them in sysctl.conf so they are preserved.
if [[ ! -f /etc/sysctl.conf || `grep -sc "kern.*.shm" /etc/sysctl.conf` -eq 0 ]]; then
    case "$OS" in
        Linux)
        echo "# kernel.shm* settings added by MagLev installation" > /tmp/sysctl.conf.$$
        echo "kernel.shmmax=`cat /proc/sys/kernel/shmmax`" >> /tmp/sysctl.conf.$$
        echo "kernel.shmall=`cat /proc/sys/kernel/shmall`" >> /tmp/sysctl.conf.$$
        ;;
        MacOSX)
        # On Mac OS X Leopard, you must have all five settings in sysctl.conf
        # before they will take effect.
        echo "# kern.sysv.shm* settings added by MagLev installation" > /tmp/sysctl.conf.$$
        sysctl kern.sysv.shmmax kern.sysv.shmall kern.sysv.shmmin kern.sysv.shmmni \
        kern.sysv.shmseg  | tr ":" "=" | tr -d " " >> /tmp/sysctl.conf.$$
        ;;
        Solaris-x86)
        # Do nothing in Solaris-x86 since /etc/sysctl.conf is ignored on Solaris 10.
        # Must configure shared memory settings manually.
        ;;
        *)
        echo "[Error] Can't determine operating system. Check script."
        exit 1
        ;;
    esac
    # Do nothing on Solaris-x86 since /etc/sysctl.conf is ignored on Solaris 10.
    if [[ "$OS" != "Solaris-x86" ]]; then
        echo "[Info] Adding the following section to /etc/sysctl.conf"
        cat /tmp/sysctl.conf.$$
        sudo bash -c "cat /tmp/sysctl.conf.$$ >> /etc/sysctl.conf"
        /bin/rm -f /tmp/sysctl.conf.$$
    fi
else
    echo "[Info] The following shared memory settings already exist in /etc/sysctl.conf"
    echo "To change them, remove the following lines from /etc/sysctl.conf and rerun this script"
    grep "kern.*.shm" /etc/sysctl.conf
fi

# Now setup for NetLDI in case we ever need it.
echo "[Info] Setting up GemStone netldi service port"
if [ `grep -sc "^gs64ldi" /etc/services` -eq 0 ]; then
    echo '[Info] Adding "gs64ldi  50378/tcp" to /etc/services'
    sudo bash -c 'echo "gs64ldi         50378/tcp        # Gemstone netldi"  >> /etc/services'
else
    echo "[Info] GemStone netldi service port is already set in /etc/services"
    echo "To change it, remove the following line from /etc/services and rerun this script"
    grep "^gs64ldi" /etc/services
fi

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
