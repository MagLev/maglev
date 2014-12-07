#! /usr/bin/env bash

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

# Defaults
export MAGLEV_SOURCE=$PWD
export MAGLEV_HOME=$PWD
export GEMSTONES_HOME=$PWD/..
export ARCHIVES_HOME=$PWD/..
export BACKUPS_HOME=$PWD/backups
export DISABLE_INSTALL_DOC=0
export SKIP_RC_REMINDER=0
export SKIP_STONE_START=0
export RUN_STWRAPPERS=0
export STONENAME
if [[ -z "$STONENAME" && -s .stonename && -f .stonename ]]
then STONENAME="$(cat .stonename)"
else STONENAME="${STONENAME:-maglev}"
fi

# Parse parameters
while (( $# ))
do
  case "$1" in
    (--prefix)
      MAGLEV_HOME="$2"
      shift 2
      ;;
    (--gemstones)
      GEMSTONES_HOME="$2"
      shift 2
      mkdir -p "$GEMSTONES_HOME"
      ;;
    (--archives)
      ARCHIVES_HOME="$2"
      shift 2
      mkdir -p "$ARCHIVES_HOME"
      ;;
    (--backups)
      BACKUPS_HOME="$2"
      shift 2
      ;;
    (--disable-install-doc)
      DISABLE_INSTALL_DOC=1
      shift
      ;;
    (--skip-rc-reminder)
      SKIP_RC_REMINDER=1
      shift
      ;;
    (--skip-stone-start)
      SKIP_STONE_START=1
      shift
      ;;
    (--run-stwrappers)
      RUN_STWRAPPERS=1
      shift
      ;;
    (--stonename)
      STONENAME="$2"
      shift 2
      ;;
    (--help)
      printf "%b" "Usage:\n./update.sh [--prefix <prefix>] [--gemstone <gemstone>] [--archives <archives] [--backups <backups>] [--disable-install-doc] [--skip-rc-reminder] [--skip-stone-start] [--run-stwrappers] [--stonename <stonename>]\n"
      exit 0
      ;;
    (*)
      echo "[ERROR] Unknown flag '$1'."
      printf "%b" "Usage:\n./update.sh [--prefix <prefix>] [--gemstone <gemstone>] [--archives <archives] [--backups <backups>] [--disable-install-doc] [--skip-rc-reminder] [--skip-stone-start] [--run-stwrappers] [--stonename <stonename>]\n"
      exit 1
      ;;
  esac
done

echo "$STONENAME" > .stonename

# check if it's proper maglev source
[[ -x "${MAGLEV_SOURCE}/bin/maglev-ruby" ]] || {
  echo "[Error] $PWD is not a valid MagLev directory"
  echo "To fix this, 'clone git://github.com:MagLev/maglev.git'"
  echo "then run install.sh from the resulting directory."
  exit 1
}

# Check that the parent directory is writable
[[ -w "$GEMSTONES_HOME" ]] || {
  echo "[Error] This script requires write permission on the MagLev parent directory."
  /bin/ls -ld "$GEMSTONES_HOME"
  echo "To fix this run: chmod u+w \"$GEMSTONES_HOME\""
  exit 1
}

# We should run this as a normal user, not root.
[[ $UID -ge 0 ]] || {
  echo "[Error] This script should be run as a normal user, not root."
  exit 1
}

function ensure()
{
  "$@" || {
    typeset _ret=$?
    echo "[Error] execution failed for '$*' with error ${_ret}."
    exit ${_ret}
  }
}

source ./build_functions.sh

# Detect operating system
PLATFORM="`uname -sm | tr ' ' '-'`"
# Macs with Core i7 use the same software as older Macs
[[ $PLATFORM == "Darwin-x86_64" ]] && PLATFORM="Darwin-i386"

gsvers=`grep ^GEMSTONE version.txt | cut -f2 -d-`
gss_name="GemStone-${gsvers}.${PLATFORM}"
gss_file="${ARCHIVES_HOME}/${gss_name}.tar.gz"

# We're good to go. Let user know.
machine_name="`uname -n`"
echo "[Info] Installing $gss_name on $machine_name"

# Look for either wget or curl to download GemStone
if [ -e "`which wget 2>/dev/null`" ]; then
    cmd="`which wget`"
elif [ -e "`which curl 2>/dev/null`" ]; then
    cmd="`which curl` -s -O"
else
    echo "[Error] Neither wget nor curl is available. Install one of them and rerun this script."
    exit 1
fi

# Download appropriate version of GemStone
if
  [[ -e $gss_file ]]
then
  echo "[Info] $gss_file already exists"
  echo "to replace it, remove or rename it and rerun this script"
else
  # Look for either wget or curl to download GemStone
  if
    [[ -e "`which wget 2>/dev/null`" ]]
  then
    cmd="`which wget` --quiet"
  elif
    [[ -e "`which curl 2>/dev/null`" ]]
  then
    cmd="`which curl` -s -O"
  else
    echo "[Error] Neither wget nor curl is available. Install one of them and rerun this script."
    exit 1
  fi
  echo "[Info] Downloading $gss_file using ${cmd}"
  ensure $cmd http://seaside.gemtalksystems.com/maglev/$gss_file
fi

# Uncompress the downloaded GemStone archive in the current directory
if
  [[ -e "$gss_name" ]]
then
  echo "[Warning] $gss_name already exists"
  echo "to replace it, remove or rename it and rerun this script"
else
  echo "[Info] Uncompressing $gss_file in $PWD"
  ensure tar xzf "$gss_file"
fi

# Create a link to the GemStone directory
echo "[Info] Linking $gss_name to ${MAGLEV_SOURCE}/gemstone"
rm -f "$MAGLEV_SOURCE/gemstone"
ln -sf "${GEMSTONES_HOME}/$gss_name" "$MAGLEV_SOURCE/gemstone"

# Finally get back to the MagLev directory
builtin cd "${MAGLEV_SOURCE}"

# Make sure we have a locks directory
mkdir -p locks
# and the correct updated keyfile
rm -f etc/maglev.demo.key
ln -sf maglev.demo.key-$PLATFORM etc/maglev.demo.key
# Make sure we have specs and benchmarks.
echo "[Info] updating MSpec and RubySpec submodules"
ensure git submodule --quiet update --init

# Check for existence of required executable rake
if
  which rake >/dev/null 2>&1
then
  # Backup any existing maglev repository
  if
    [[ -e "${MAGLEV_HOME}/data/maglev/extent/extent0.ruby.dbf" ]]
  then
    [[ "$BACKUPS_HOME" == "$PWD/backups" ]] || {
      rm -rf backups
      ln -s "$BACKUPS_HOME/" backups
    }
    echo "[Info] Backing up existing 'maglev' repository to $BACKUPS_HOME/previous_maglev_extent.tgz"
    rake maglev:take_snapshot >/dev/null
    mv backups/maglev_extent.tgz backups/previous_maglev_extent.tgz
  fi
else
  echo "[Warning] rake not found!"
  echo "Skipping creation of default 'maglev' repository and HTML documentation."
fi

[[ "$MAGLEV_SOURCE" == "$MAGLEV_HOME" ]] || {
  echo "[Info] Coping MagLev to target location $MAGLEV_HOME"
  if [[ -d "$MAGLEV_HOME" ]]
  then rm -rf "$MAGLEV_HOME"
  fi
  cp -rf "$MAGLEV_SOURCE" "$MAGLEV_HOME"
  # Make sure we are in MagLev target directory
  builtin cd "${MAGLEV_SOURCE}"
}

# setup topaz environment
export GEMSTONE GEMSTONE_GLOBAL_DIR GEMSTONE_SYS_CONF GEMSTONE_DATADIR GEMSTONE_LOG
GEMSTONE="${MAGLEV_HOME}/gemstone"
GEMSTONE_GLOBAL_DIR="$MAGLEV_HOME"
GEMSTONE_SYS_CONF="${MAGLEV_HOME}/etc/system.conf"
GEMSTONE_DATADIR="${MAGLEV_HOME}/data/${STONENAME}"
GEMSTONE_LOG="${MAGLEV_HOME}/log/${STONENAME}/${STONENAME}.log"

# Create a default repository called "maglev" and generate the MagLev HTML documentation
#TODO: stop stone if running

# create a clean slate
if
  [[ -e etc/conf.d/maglev.conf ]]
then
  echo "[Info] Removing existing '${STONENAME}' configuration file."
  rm -rf "${GEMSTONE_DATADIR}"
  rm -rf "${MAGLEV_HOME}/etc/conf.d/${STONENAME}.conf"
  rm -rf "${MAGLEV_HOME}/log/${STONENAME}"
fi
if
  [[ -e "${MAGLEV_HOME}/bin/extent0.ruby.dbf" ]]
then
  [[ -e etc/conf.d/maglev.conf ]] || build_maglev_stone_create
else
  extent0='gemstone/bin/extent0.dbf'
  if
    [[ -e $extent0 ]]
  then
    if
      build_maglev
    then
      if
        [[ $DISABLE_INSTALL_DOC == 0 ]] &&
        [[ -x "${MAGLEV_HOME}/bin/rake" ]]
      then
        echo "[Info] Generating the MagLev HTML documentation"
        env GEM_HOME="" GEM_PATH="${MAGLEV_HOME}/lib/maglev/gems/1.8/gems" \
          "${MAGLEV_HOME}/bin/rake" rdoc >/dev/null 2>&1
      fi
    else
      echo "[Warning] Could not build new ruby extent"
    fi
  else
    echo "[Warning] Can't find ${extent0}: Skip building ruby extent"
  fi
fi

if
  [[ $RUN_STWRAPPERS == 1 ]]
then
  #rake stwrappers
  wrapper_dir="${MAGLEV_HOME}/lib/ruby/site_ruby/1.8/smalltalk"
  if
    [[ -e "${wrapper_dir}" ]]
  then
    echo "[Info] Smalltalk FFI already exist."
  else
    echo "[Info] Generating smalltalk FFI."
    #TODO: what was the % supposed to do?
    run_on_stone "omit resultcheck" "run" "RubyContext createSmalltalkFFIWrappers" "%"
  fi
fi

if
  [[ $SKIP_STONE_START == 0 ]]
then
  echo "[Info] Starting MagLev stone (loading kernel classes)"
  "$MAGLEV_HOME/bin/maglev" start
fi

echo
echo "[Info] Finished upgrade to $gss_name on $machine_name"
echo ""
echo "[Info] MagLev version information:"
cat version.txt
echo "[Info] GemStone version information:"
cat gemstone/version.txt

# Reminder to setup environment variables
if
  [[ $SKIP_RC_REMINDER == 0 ]]
then
  echo ""
  echo "[Info] Adding these to your .bashrc will make it easier to run MagLev"
  echo "export MAGLEV_HOME=${PWD}"
  echo 'export PATH=$MAGLEV_HOME/bin:$PATH'
fi

# Reminder to generate Smalltalk FFI
if
  [[ $RUN_STWRAPPERS == 0 ]]
then
  echo ""
  echo "[Info] If you want to call GemStone Smalltalk methods from Ruby, run"
  echo "  rake stwrappers"
  echo "after this upgrade has finished. This will generate .rb files you can use"
  echo "in \$MAGLEV_HOME/lib/ruby/site_ruby/1.8/smalltalk/"
fi
