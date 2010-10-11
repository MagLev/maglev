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
    args.with_defaults :rackup_file => 'config.ru', :version => '1.8.7'
    sh "source $HOME/.rvm/scripts/rvm ; rvm #{args[:version]} && rackup #{RACKUP_OPTS} --port 3333 #{args[:rackup_file]}"
  end

  desc "run maglev httpd"
  task :maglev, :rackup_file do |t, args|
    bail_if_rvm_hosing_environment
    args.with_defaults :rackup_file => 'config.ru'
    sh "$MAGLEV_HOME/bin/rackup #{RACKUP_OPTS} --port 3333 #{args[:rackup_file]}"
  end
end

namespace :lighttpd do
  directory 'log'

  desc "start lighttpd listening for SCGI on port 3000"
  task :scgi, :config, :needs => 'log' do |t, args|
    args.with_defaults :config => 'config/lighttpd.conf'
    sh "lighttpd -D -f #{args[:config]}"
  end

  desc "start lighttpd listening for multiple SCGIs on port 3000-3001"
  task :scgimulti do
    Rake::Task['lighttpd:scgi'].invoke 'config/lighttpd-multi-scgi.conf'
  end

  desc "BROKEN pending Trac799: start lighttpd listening for FCGI on port 3000"
  task :fcgi => 'log' do
    sh "lighttpd -D -f config/lighttpd-fcgi.conf"
  end
end

# Start Ruby SCGI servers so lighttpd can connect to them
namespace :scgi do
  SCGI_PORT = 3000
  RACKUP_SCGI_OPTS = "#{RACKUP_OPTS} --server SCGI"

  desc "Start count maglev VMs + scgi (default 1)"
  task :maglev, :rackup_file, :count do |t, args|
    args.with_defaults :rackup_file => 'config.ru', :count => '1'
    args[:count].to_i.times do |i|
      port = SCGI_PORT + i
      my_args = "#{RACKUP_SCGI_OPTS} --pid rack-#{port}.pid --port #{port}"
      sh "nohup #{MAGLEV_HOME}/bin/rackup #{my_args} #{args[:rackup_file]} &"
      sleep 1
    end
  end

  desc "1 MRI VM + scgi: version is like 1.8.7 or 1.9.2"
  task :mri, :rackup_file, :version do |t, args|
    args.with_defaults :rackup_file => 'config.ru', :version => '1.8.7'
    sh "source $HOME/.rvm/scripts/rvm ; rvm #{args[:version]} && rackup #{RACKUP_SCGI_OPTS} #{args[:rackup_file]}"
  end

  desc "kill SCGI apps named in rack-*.pid"
  task :kill, :signal do |t, args|
    args.with_defaults :signal => 'TERM'
    Dir.glob('rack-*.pid').each do |pid_file|
      begin
        pid = File.readlines(pid_file)
        sh "kill -s #{args[:signal]} #{pid}"
      rescue
        puts "Failed on file #{pid_file}  pid #{pid}"
      ensure
        rm_f pid_file
      end
    end
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
  desc "ab for count queries (default 5_000) on http://127.0.0.1:3333/app/magtag.css"
  task :ab, :count do |t, args|
    args.with_defaults :count => 5_000
    puts "\n\n==== ab with #{args[:count]} queries ===============\n\n"
    sh "curl http://localhost:3333/info"
    sh "ab -n #{args[:count]} -c 10 http://127.0.0.1:3333/magtag.css"
  end
end
