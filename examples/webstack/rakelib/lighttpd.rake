# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

namespace :lighttpd do
  desc 'start lighttpd listening for n SCGI servers starting at port 3000.  This simply
        starts lighttpd with config file "config/lighttpd-scgi-#{n}.conf".'
  task :scgi, :n, :needs => 'log' do |t, args|
    args.with_defaults :n => '1'
    config = "config/lighttpd-scgi-#{args[:n]}.conf"
    raise "No config file for #{args[:n]}: #{config}" unless File.exist? config
    sh "lighttpd -D -f #{config}"
  end

  desc "start lighttpd with a config file (default config/lighttpd-scgi-1.conf)"
  task :scgiconfig, :config, :needs => 'log' do |t, args|
    args.with_defaults :config => 'config/lighttpd-scgi-1.conf'
    sh "lighttpd -D -f #{args[:config]}"
  end

  desc "BROKEN pending Trac799: start lighttpd listening for FCGI on port 3000"
  task :fcgi => 'log' do
    sh "lighttpd -D -f config/lighttpd-fcgi-1.conf"
  end

  desc "Kill any lighttpd instances running"
  task :kill! do
    kill_from_pidfile 'log/lighttpd.pid'
  end
end
