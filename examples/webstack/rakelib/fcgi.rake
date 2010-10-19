# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

namespace :fcgi do
  FCGI_PORT = 3000
  RACKUP_FCGI_OPTS = "#{RACKUP_OPTS} --server FastCGI"
  desc "1 maglev VM + scgi"
  task :maglev => :log do
    opts = "#{RACKUP_FCGI_OPTS} --pid log/fcgi-#{FCGI_PORT}.pid --port #{FCGI_PORT}"
    sh "#{MAGLEV_HOME}/bin/rackup #{opts} config/fcgi.ru"
  end
end
