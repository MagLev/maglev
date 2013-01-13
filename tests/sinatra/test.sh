#!/bin/bash -ex
#
# Based on the Jenkins script From RKH at
# http://ci.rkh.im/job/sinatra-maglev/

PATH=$MAGLEV_HOME/bin:$PATH
MAGLEV_OPTS=

rm -rf sinatra
git clone --depth 1 https://github.com/sinatra/sinatra.git

cd sinatra

git submodule init
git submodule update --init --recursive

if [ -z "$TRAVIS" ]; then
    # Our corporate firewall does not let us use git: protocol.  So,
    # we patch Sinatra Gemfile to use http: rather than git:.  Also,
    # we use perl rather than sed because Solaris sed is broken (no
    # -i).
    echo "Patching sinatra/Gemfile"
    perl -pi -e s/git:/http:/ Gemfile
    echo "source 'http://w2-stdev-ub10-01.gemstone.com:9292'"|cat - Gemfile > Gemfile.new
    mv Gemfile.new Gemfile
fi

export rack=master

# In the Jenkins environment, $WORKSPACE will be set.
# We need to start maglev if we're under jenkins.
if [[ -n $WORKSPACE ]]; then
    mkdir -p "${WORKSPACE}/reports"
    maglev start
fi

# Install gems that are required but need patches
maglev-gem install eventmachine json nokogiri multi_json yajl-ruby mongrel bcrypt-ruby

# consider adding  --without-coffee-script for Allen
# maglev-ruby -S bundle install --without-coffee-script
maglev-ruby -S bundle install
maglev-ruby -S bundle update
bundle exec rake -Ilib ci:setup:testunit test

# Work around Maglev ci reporter builder bug
# cd test/reports/
# for i in *.xml; do
#   cat "$i" | tail -n +2 > "${WORKSPACE}/reports/$i"
# done

if [[ -n $WORKSPACE ]]; then
    maglev stop
fi
