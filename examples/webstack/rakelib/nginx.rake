# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

directory 'log/nginx'

DIR = File.dirname(File.dirname(__FILE__)) + '/'

namespace :nginx do
  desc 'start nginx listening for n MagLev servers starting at port 3000.
        This simply starts nginx with config file "config/nginx-#{n}.conf".'
  task :proxy, :n, :needs => 'log/nginx' do |t, args|
    args.with_defaults :n => '1'
    config = "config/nginx-#{args[:n]}.conf"
    raise "No config file for #{args[:n]}: #{config}" unless File.exist? config
    sh "nginx -p #{DIR} -c #{config}"
  end

  # desc "start nginx with a config file (default config/nginx-scgi-1.conf)"
  # task :scgiconfig, :config, :needs => 'log' do |t, args|
  #   args.with_defaults :config => 'config/nginx-scgi-1.conf'
  #   sh "nginx -D -f #{args[:config]}"
  # end

  desc "Kill any nginx instances running"
  task :kill! do
    # Pick one of the config files, all we need is the pid directive, and
    # it is the same in all of the nginx config files.
    sh "nginx -p #{DIR} -c config/nginx-1.conf -s quit"
  end
end
