#! /usr/bin/env bash

#=========================================================================
# Copyright (C) GemStone Systems, Inc. 2010.
#
# Name - install.sh
#
# Purpose - Make sure MagLev and GemStone are setup correctly in a fresh clone
#           of MagLev from git://github.com:MagLev/maglev.git. You only need
#           to run this once, you can use update.sh from then on.
#
# $Id:$
#
# Actions:
#    Invoke validate.sh to verify if machine is capable of running MagLev
#    Invoke setup.sh to configure system for MagLev
#    Invoke update.sh to finish the job
#=========================================================================

bash ./validate.sh &&
bash ./setup.sh &&
bash ./update.sh "$@" ||
exit $?
