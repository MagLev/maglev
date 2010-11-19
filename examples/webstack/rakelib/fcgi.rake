# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

namespace :fcgi do
  desc "Start MagLev VM(s) + fcgi with lighttpd bug workaround"
  task :lighttpdbug, :count, :needs => :log do |t,args|
    args.with_defaults :count => '1'
    rackup_on_ports("#{RACKUP_OPTS} --server FastCGI", "config/lighttpd-fcgi.ru", 'fcgi', args[:count].to_i)
  end
end
