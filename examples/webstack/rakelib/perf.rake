# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

namespace :webrick do
  # TODO: DRY :webrick and :scgi....there is much similar
  desc "run MRI httpd"
  task :mri, :rackup_file, :version do |t, args|
    rackup_file = args[:rackup_file] || 'config.ru'
    mri_version = args[:version]     || '1.8.7'
    sh "source $HOME/.rvm/scripts/rvm ; rvm #{version} && rackup #{RACKUP_OPTS} --port 3333 #{rackup_file}"
  end

  desc "run maglev httpd"
  task :maglev, :rackup_file do |t, args|
    bail_if_rvm_hosing_environment
    rackup_file = args[:rackup_file] || 'config.ru'
    sh "$MAGLEV_HOME/bin/rackup #{RACKUP_OPTS} --port 3333 #{rackup_file}"
  end
end

namespace :lighttpd do
  directory 'log'

  desc "start lighttpd listening for SCGI on port 3000"
  task :scgi => 'log' do
    sh "lighttpd -D -f config/lighttpd.conf"
  end

  desc "BROKEN pending Trac799: start lighttpd listening for FCGI on port 3000"
  task :fcgi => 'log' do
    sh "lighttpd -D -f config/lighttpd-fcgi.conf"
  end
end

# Start Ruby SCGI servers so lighttpd can connect to them
namespace :scgi do
  SCGI_PORT = 3000
  RACKUP_SCGI_OPTS = "#{RACKUP_OPTS} --server SCGI --pid rack-#{SCGI_PORT}.pid --port #{SCGI_PORT}"

  desc "1 maglev VM + scgi"
  task :maglev, :rackup_file do |t, args|
    rackup_file = args[:rackup_file] || 'config.ru'
    sh "#{MAGLEV_HOME}/bin/rackup #{RACKUP_SCGI_OPTS} #{rackup_file}"
  end

  desc "1 MRI VM + scgi: version is like 1.8.7 or 1.9.2"
  task :mri, :rackup_file, :version do |t, args|
    rackup_file = args[:rackup_file] || 'config.ru'
    mri_version = args[:version]     || '1.8.7'
    sh "source $HOME/.rvm/scripts/rvm ; rvm #{mri_version} && rackup #{RACKUP_SCGI_OPTS} #{rackup_file}"
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
  task :ab, :count do |t, args|
    count = args[:count] || 5_000
    puts "\n\n==== ab with #{count} queries ===============\n\n"
    sh "curl http://localhost:3333/info"
    sh "ab -n #{count} -c 10 http://127.0.0.1:3333/magtag.css"
  end
end
