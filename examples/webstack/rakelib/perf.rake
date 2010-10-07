# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

namespace :webrick do
  desc "run 1.8.7 httpd"
  task :mri187 do
    sh "source $HOME/.rvm/scripts/rvm ; rvm 1.8.7 && rackup #{RACKUP_OPTS} --port 3333 config.ru"
  end

  desc "run 1.9.2 httpd"
  task :mri192 do
    sh "source $HOME/.rvm/scripts/rvm ; rvm 1.9.2 && rackup #{RACKUP_OPTS} --port 3333 config.ru"
  end

  desc "run maglev httpd"
  task :maglev do
    bail_if_rvm_hosing_environment
    sh "$MAGLEV_HOME/bin/rackup #{RACKUP_OPTS} --port 3333 config.ru"
  end
end

namespace :lighttpd do
  directory 'log'

  desc "start lighttpd listening for SCGI on port 3000"
  task :scgi => 'log' do
    sh "lighttpd -D -f config/lighttpd.conf"
  end

  desc "start lighttpd listening for FCGI on port 3000"
  task :fcgi => 'log' do
    sh "lighttpd -D -f config/lighttpd-fcgi.conf"
  end
end

# Start Ruby SCGI servers so lighttpd can connect to them
namespace :scgi do

  SCGI_PORT = 3000
  RACKUP_SCGI_OPTS = "#{RACKUP_OPTS} --server SCGI"

  desc "1 maglev VM + scgi"
  task :maglev do
    opts = "#{RACKUP_SCGI_OPTS} --pid rack-#{SCGI_PORT}.pid --port #{SCGI_PORT}"
    sh "#{MAGLEV_HOME}/bin/rackup #{opts} config.ru"
  end

  desc "1 MRI 1.8.7 VM + scgi"
  task :mri187 do
    opts = "#{RACKUP_SCGI_OPTS} --pid rack-#{SCGI_PORT}.pid --port #{SCGI_PORT}"
    sh "source $HOME/.rvm/scripts/rvm ; rvm 1.8.7 && rackup #{opts} config.ru"
  end

  desc "1 MRI 1.9.2 VM + scgi"
  task :mri192 do
    opts = "#{RACKUP_SCGI_OPTS} --pid rack-#{SCGI_PORT}.pid --port #{SCGI_PORT}"
    sh "source $HOME/.rvm/scripts/rvm ; rvm 1.9.2 && rackup #{opts} config.ru"
  end
end

namespace :fcgi do
  FCGI_PORT = 3000
  RACKUP_FCGI_OPTS = "#{RACKUP_OPTS} --server FastCGI"
  desc "1 maglev VM + scgi"
  task :maglev do
    opts = "#{RACKUP_FCGI_OPTS} --pid rack-#{FCGI_PORT}.pid --port #{FCGI_PORT}"
    sh "#{MAGLEV_HOME}/bin/rackup #{opts} config-fcgi.ru"
  end
end

namespace :client do
  desc "ab for 10_000 queries on http://127.0.0.1:3333/magtag.css"
  task :ab do
    puts "\n\n=============================================================\n\n"
    sh "curl http://localhost:3333/info"
    sh "ab -n 10000 -c 10 http://127.0.0.1:3333/magtag.css"
  end
end

