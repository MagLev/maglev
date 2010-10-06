# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

namespace :server do
  desc "run 1.8.7 httpd"
  task :mri187 do
    sh "source $HOME/.rvm/scripts/rvm ; rvm 1.8.7 && rackup #{RACKUP_OPTS} config.ru"
  end

  desc "run 1.9.2 httpd"
  task :mri192 do
    sh "source $HOME/.rvm/scripts/rvm ; rvm 1.9.2 && rackup #{RACKUP_OPTS} config.ru"
  end

  desc "run maglev httpd"
  task :maglev do
    bail_if_rvm_hosing_environment
    sh "$MAGLEV_HOME/bin/rackup #{RACKUP_OPTS} config.ru"
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

