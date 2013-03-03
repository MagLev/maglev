#! /bin/bash

#=========================================================================
# Copyright (C) GemStone Systems, Inc. 2010.
#
# Name - install.sh
#
# Purpose - Make sure MagLev and GemStone are setup correctly in a fresh clone
#           of MagLev from git://github.com:MagLev/maglev.git. You only need 
#           to run this once, you can use update.sh from then on.
#
#           Be both verbose and idempotent, so we can easily diagnose
#           any problems.
#
# $Id:$
#
# Description:
#    Does a basic setup of memory and services for MagLev 
#    Setup for manual MagLev startup rather than automatic startup upon boot
#    Safe to run multiple times. Only saves one prior backup repository though.
#    Requires root access (using sudo) to change setings and create directories
#
# Actions:
#    Verify machine is capable of running MagLev 64-bit
#    Add shared memory setup to /etc/sysctl.conf
#    Add GemStone netldi service port to /etc/services
#    Invoke update.sh to finish the job
#=========================================================================

# Parse parameters
PREBUILT_STONE=0
INPLACE_UPGRADE=1
GENERATE_RDOC=1
RUN_STWRAPPERS=1

while (( $# ))
do
    case "$1" in
	(--no-in-place-upgrade)
	    INPLACE_UPGRADE=0
	    shift
	    ;;
	(--prebuilt-stone)
	    PREBUILT_STONE=1
	    INPLACE_UPGRADE=0
	    shift
	    ;;
	(--disable-install-doc)
	    GENERATE_RDOC=0
	    shift
	    ;;
	(--run-stwrappers)
	    RUN_STWRAPPERS=0
	    shift
	    ;;
	(--help)
	    echo "You can pass the following options:"
	    echo "  --no-in-place-upgrade    Do not attempt to upgrade an existing stone"
	    echo "  --use-prebuilt-stone     Use the existing maglev stone (for installs without rake)"
	    echo "  --disable-install-doc    Don't install rdoc documentation (Default: yes)"
	    echo "  --run-stwrappers         Create wrappers for Smalltalk classes (Default: no)"
	    exit 0
	    ;;
    esac
done


function set_maglev_home {
    if [ -x bin/maglev-ruby ]; then
        # echo "using $PWD as MAGLEV_HOME"
	export MAGLEV_HOME=$PWD
    else
	echo "[Error] $PWD is not a valid MagLev directory"
	echo "To fix this, 'clone git://github.com:MagLev/maglev.git'"
	echo "then run install.sh from the resulting directory."
	exit 1
    fi
}

function pre_install_checks {
    # Check that the parent directory is writable
    if [ ! -w ".." ]; then
	echo "[Error] This script requires write permission on the MagLev parent directory."
	/bin/ls -ld ..
	echo "To fix this, 'chmod u+w ..'"
	exit 1
    fi

    detect_os
    shmmem_setup
    netldi_setup
}

function detect_os {
    PLATFORM="`uname -sm | tr ' ' '-'`"
    # Macs with Core i7 use the same software as older Macs
    [ $PLATFORM = "Darwin-x86_64" ] && PLATFORM="Darwin-i386"

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
	    if [[ $CPU_TYPE != "i386" || $CPU_CAPABLE -ne 1 ||
			$MAJOR -lt 10 || $MINOR -lt 5 ]] ; then
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
	    echo "[Error] This script only works on 64-bit Linux, Mac OS X, or Solaris-x86"
	    echo "The result from \"uname -sm\" is \"`uname -sm`\""
	    exit 1
	    ;;
    esac
}

# We're good to go. Let user know.
machine_name="`uname -n`"
echo "[Info] Configuring $machine_name to run MagLev"

function shmmem_setup {
    # Figure out how much total memory is installed
    echo "[Info] Making sure shared memory is set up"
#
# Ref: http://wiki.finkproject.org/index.php/Shared_Memory_Regions_on_Darwin
# Ref: http://developer.postgresql.org/pgdocs/postgres/kernel-resources.html
# Ref: http://www.idevelopment.info/data/Oracle/DBA_tips/Linux/LINUX_8.shtml
#
    case "$PLATFORM" in
	Linux-x86_64)
    # use TotalMem: kB because Ubuntu doesn't have Mem: in Bytes
	    totalMemKB=`awk '/MemTotal:/{print($2);}' /proc/meminfo`
	    totalMem=$(($totalMemKB * 1024))
    # Figure out the max shared memory segment size currently allowed
	    shmmax=`cat /proc/sys/kernel/shmmax`
    # Figure out the max shared memory currently allowed
	    shmall=`cat /proc/sys/kernel/shmall`
	    ;;
	Darwin-i386)
	    totalMem="`sysctl hw.memsize | cut -f2 -d' '`"
    # Figure out the max shared memory segment size currently allowed
	    shmmax="`sysctl kern.sysv.shmmax | cut -f2 -d' '`"
    # Figure out the max shared memory currently allowed
	    shmall="`sysctl kern.sysv.shmall | cut -f2 -d' '`"
	    ;;
	SunOS-i86pc)
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
	[ $PLATFORM = "Darwin-i386" ] && sudo sysctl -w kern.sysv.shmmax=$shmmaxNew
	[ $PLATFORM = "Linux-x86_64" ] && sudo bash -c "echo $shmmaxNew > /proc/sys/kernel/shmmax"
	[ $PLATFORM = "SunOS-i86pc" ] && echo "[Warning] shmmax must be set manually on Solaris-x86"
    else
	echo "[Info] No need to increase max shared memory segment size"
    fi

# Increase shmall if appropriate
    if [ $shmallNew -gt $shmall ]; then
	echo "[Info] Increasing max shared memory allowed to $shmallNewMB MB"
	[ $PLATFORM = "Darwin-i386" ] && sudo sysctl -w kern.sysv.shmall=$shmallNew
	[ $PLATFORM = "Linux-x86_64" ] && sudo bash -c "echo $shmallNew > /proc/sys/kernel/shmall"
	[ $PLATFORM = "SunOS-i86pc" ] && echo "[Warning]shmall must be set manually on Solaris-x86"
    else
	echo "[Info] No need to increase max shared memory allowed"
    fi

# At this point, shared memory settings contain the values we want, 
# put them in sysctl.conf so they are preserved.
    if [[ ! -f /etc/sysctl.conf || `grep -sc "kern.*.shm" /etc/sysctl.conf` -eq 0 ]]; then
	case "$PLATFORM" in
            Linux-x86_64)
		echo "# kernel.shm* settings added by MagLev installation" > /tmp/sysctl.conf.$$
		echo "kernel.shmmax=`cat /proc/sys/kernel/shmmax`" >> /tmp/sysctl.conf.$$
		echo "kernel.shmall=`cat /proc/sys/kernel/shmall`" >> /tmp/sysctl.conf.$$
		;;
            Darwin-i386)
        # On Mac OS X Leopard, you must have all five settings in sysctl.conf
        # before they will take effect.
		echo "# kern.sysv.shm* settings added by MagLev installation" > /tmp/sysctl.conf.$$
		sysctl kern.sysv.shmmax kern.sysv.shmall kern.sysv.shmmin kern.sysv.shmmni \
		    kern.sysv.shmseg  | tr ":" "=" | tr -d " " >> /tmp/sysctl.conf.$$
		;;
            SunOS-i86pc)
        # Do nothing in Solaris-x86 since /etc/sysctl.conf is ignored on Solaris 10.
        # Must configure shared memory settings manually.
		;;
            *)
		echo "[Error] Can't determine operating system. Check script."
		exit 1
		;;
	esac
    # Do nothing on Solaris-x86 since /etc/sysctl.conf is ignored on Solaris 10.
	if [[ "$PLATFORM" != "SunOS-i86pc" ]]; then
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
}

function netldi_setup {
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
}

function download_gemstone {
# Detect operating system
    PLATFORM="`uname -sm | tr ' ' '-'`"
# Macs with Core i7 use the same software as older Macs
    [ $PLATFORM = "Darwin-x86_64" ] && PLATFORM="Darwin-i386"
    gsvers=`grep ^GEMSTONE version.txt | cut -f2 -d-`
    gss_name="GemStone-${gsvers}.${PLATFORM}"
    gss_file=${gss_name}.tar.gz

# Look for either wget or curl to download GemStone
    if [ -e "`which wget 2>/dev/null`" ]; then
	cmd="`which wget`"
    elif [ -e "`which curl 2>/dev/null`" ]; then
	cmd="`which curl` -s -O"
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
}

function directory_setup {
# Make sure we have a locks directory
    mkdir -p locks
# and the correct updated keyfile
    rm -f etc/maglev.demo.key
    ln -sf maglev.demo.key-$PLATFORM etc/maglev.demo.key
# Make sure we have specs and benchmarks.
    echo "[Info] updating MSpec and RubySpec submodules"
    git submodule --quiet update --init
}

function set_rake {
# Check for existence of required executable rake
    if [  -e "`which rake 2>/dev/null`" ]; then
	if [ "$(dirname `which rake`)" == "${MAGLEV_HOME}/bin" ]; then
            echo "[Error] Cannot use the rake in \$MAGLEV_HOME/bin to update. Please put another Ruby's rake in your PATH."
            exit 1
	fi
    else
	echo "[Error] rake not found!"
	echo "Skipping creation of default 'maglev' repository and HTML documentation."
    fi
    RAKE="`which rake`"
}

function backup {
    if [ $PREBUILT_STONE == 0 ]; then
	set_rake
    # Backup any existing maglev repository
	if [ -e data/maglev/extent/extent0.ruby.dbf ]; then
            echo "[Info] Backing up existing 'maglev' repository to backups/previous_maglev_extent.tgz"
            $RAKE maglev:take_snapshot >/dev/null
            mv backups/maglev_extent.tgz backups/previous_maglev_extent.tgz
	fi
    fi
}

function in_place_upgrade {
    if [ $INPLACE_UPGRADE == 1 ]; then
	set_rake
        # Backup any existing maglev repository
	if [ -e data/maglev/extent/extent0.ruby.dbf ]; then
        # Try update in place, might not work reliably
	    $RAKE maglev:reload_everything >/dev/null
            if [ $? -eq 0 ]; then
		echo "[Info] Upgraded exisiting 'maglev' repository in-place."
		echo "       Upgrade any other stones using 'rake STONENAME:take_snapshot STONENAME:reload_everything'."
		echo "           This will create a backup in \$MAGLEV_HOME/backups (your data is cleared during the upgrade)."
		echo "       In rare cases the in-place upgrade may not work correctly. If you encounter problems after this, destroy the stone an re-run this script."
		return 0
            fi
	fi
	return 1
    else
	return 1
    fi
}

function create_stone {
    if [ $PREBUILT_STONE == 0 ]; then
	set_rake
    # create a clean slate
	if [ -e etc/conf.d/maglev.conf ]; then
            echo "[Info] In-place upgrade not possible. Removing existing 'maglev' configuration file."
            $RAKE stone:destroy[maglev] >/dev/null
	fi

	$RAKE build:clobber
	extent0='gemstone/bin/extent0.dbf'
	echo "[Info] Building new extent0.ruby.dbf from $extent0 and creating default maglev stone"
	echo "This could take a while..."
	if [ -e $extent0 ]; then
        # NOTE: build:maglev will also create the maglev stone
            if $RAKE build:maglev ; then
		return 0
            else
		echo "[Error] Could not build new ruby extent. This means there was an error loading the Smalltalk code."
		return 1
            fi
	else
            echo "[Error] Can't find ${extent0}: Skip building ruby extent. This means your GemStone download is broken."
            return 1
	fi
    else
	if [ ! -e "etc/conf.d/maglev.conf" ]; then
	    echo "[Error] No prebuilt maglev stone available"
	    return 1
	else
	    bin/maglev start
	    bin/maglev-ruby src/kernel/extensions.rb
	fi
    fi
}

function generate_rdoc {
    if [ $GENERATE_RDOC == 1 ]; then
	set_rake
	echo "[Info] Generating the MagLev HTML documentation"
	$RAKE rdoc >/dev/null 2>&1
    fi
}

function start_stone {
    set_rake
    echo "[Info] Starting MagLev stone (loading kernel classes)"
    if $RAKE maglev:start ; then
        echo "[Info] Successfully loaded kernel classes"
	return 0
    else
        echo "[Error] Failed loading kernel classes!"
        return 1
    fi
}

function generate_stwrappers {
    if [ $RUN_STWRAPPERS == 1 ]; then
	echo ""
	echo "[Info] If you want to call GemStone Smalltalk methods from Ruby, run"
	echo "  rake stwrappers"
	echo "after this upgrade has finished. This will generate .rb files you can use"
	echo "in \$MAGLEV_HOME/lib/ruby/site_ruby/1.8/smalltalk/"
    else
	set_rake
	$RAKE stwrappers
    fi
}

set_maglev_home &&
pre_install_checks &&
download_gemstone &&
directory_setup &&
backup &&
( in_place_upgrade || ( create_stone && start_stone ) ) &&
generate_rdoc &&
generate_stwrappers

echo
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
