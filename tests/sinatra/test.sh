#!/bin/bash -ex
#
# Based on the Jenkins script From RKH at
# http://ci.rkh.im/job/sinatra-maglev/

PATH=$MAGLEV_HOME/bin:$PATH
MAGLEV_OPTS=

rm -rf sinatra
git clone https://github.com/sinatra/sinatra.git

cd sinatra
git submodule init
git submodule update --init --recursive

# Our corporate firewall does not let us use git: protocol.  So, we patch
# Sinatra Gemfile to use http: rather than git:.  Also, we use perl rather
# than sed because Solaris sed is broken (no -i).
echo "Patching sinatra/Gemfile"
perl -pi -e s/git:/http:/ Gemfile

export rack=master

# Patch the broken version of rack
badf=$MAGLEV_HOME/lib/maglev/gems/1.8/gems/rack-1.3.0/lib/rack/session/abstract/id.rb
if [[ -f $badf ]]; then
    echo "Patching $badf"
    perl -pi -e s/NotImpelentedError/NotImplementedError/ $badf
else
    echo "Could not find rack file to patch: $badf"
fi

# In the Jenkins environment, $WORKSPACE will be set.
# We need to start maglev if we're under jenkins.
if [[ -n $WORKSPACE ]]; then
    mkdir -p "${WORKSPACE}/reports"
    maglev start
fi

maglev-ruby -S bundle install
bundle exec rake -Ilib ci:setup:testunit test

# Work around Maglev ci reporter builder bug
# cd test/reports/
# for i in *.xml; do
#   cat "$i" | tail -n +2 > "${WORKSPACE}/reports/$i"
# done

if [[ -n $WORKSPACE ]]; then
    maglev stop
fi
