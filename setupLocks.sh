#! /bin/bash
#=========================================================================
# Copyright (C) GemStone Systems, Inc. 2011.
#
# Name - setupLocks.sh
#
# Purpose - Make sure MagLev and GemStone are both using the same locks directory
#
# $Id:$
#
# Description:
#    GemStone expects locks to be in /opt/gemstone/locks
#    MagLev expects locks to be in $MAGLEV_HOME/locks
#    Ditto for log directory
#    This script links them together
#    Your need to rerun this script anytime you change $MAGLEV_HOME
#    Use at your own risk!
#
# Actions:
#    Stop MagLev first if it is running
#    Create /opt/gemstone/locks and /opt/gemstone/log
#    Remove existing $MAGLEV_HOME/locks and $MAGLEV_HOME/log
#    Soft link respective directories to equivalent ones in /opt/gemstone

cd $MAGLEV_HOME
maglev stop

sudo mkdir -p /opt/gemstone /opt/gemstone/log /opt/gemstone/locks
sudo chown $USER:${GROUPS[0]} /opt/gemstone /opt/gemstone/log /opt/gemstone/locks
sudo chmod 775 /opt/gemstone /opt/gemstone/log /opt/gemstone/locks

rm -rf locks log

ln -s /opt/gemstone/log
ln -s /opt/gemstone/locks
