# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

directory 'log'

namespace :webrick do
  # TODO: DRY :webrick and :scgi....there is much similar
  desc "run MRI via rvm; uses webrick"
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

# Start Ruby SCGI servers so lighttpd can connect to them
namespace :scgi do
  SCGI_PORT = 3000
  RACKUP_SCGI_OPTS = "#{RACKUP_OPTS} --server SCGI"

  desc "Start MagLev SCGI VMs (default 1, 'config/lock_txn_wrap.ru')"
  task :maglev, :count, :rackup_file, :needs => ['log'] do |t, args|
    args.with_defaults :rackup_file => 'config/lock_txn_wrap.ru', :count => '1'
    args[:count].to_i.times do |i|
      port = SCGI_PORT + i
      cmd = "#{MAGLEV_HOME}/bin/rackup #{RACKUP_SCGI_OPTS} --pid log/scgi-#{port}.pid --port #{port} #{args[:rackup_file]}"
      out = "log/scgi-#{i}.out"
      sh "echo  #{cmd} >  #{out}"        # zeros output and lets us know command
      sh "nohup #{cmd} >> #{out} 2>&1 &"
      sleep 1
    end
  end

  desc "1 MRI VM + scgi: version is like 1.8.7 or 1.9.2"
  task :mri, :rackup_file, :version do |t, args|
    args.with_defaults :rackup_file => 'config.ru', :version => '1.8.7'
    sh "source $HOME/.rvm/scripts/rvm ; rvm #{args[:version]} && rackup #{RACKUP_SCGI_OPTS} #{args[:rackup_file]}"
  end

  desc "kill SCGI apps named in log/scgi-*.pid"
  task :kill!, :signal do |t, args|
    args.with_defaults :signal => 'TERM'
    files = Dir.glob('log/scgi-*.pid')
    if files.size > 0
      files.each { |f| kill_from_pidfile f }
    else
      puts "No pid files in log/"
    end
  end

  desc "List pids of SCGI servers connected on sockets"
  task :pids do
    sh "lsof -i tcp:3000-3004" do |ok, res|
      unless res.exitstatus == 1  # 1 means no pids found, i.e., OK!
        puts "Error: ok #{ok}  res #{res}  res.exitstatus #{res.exitstatus}"
      end
    end
  end
end

namespace :fcgi do
  FCGI_PORT = 3000
  RACKUP_FCGI_OPTS = "#{RACKUP_OPTS} --server FastCGI"
  desc "1 maglev VM + scgi"
  task :maglev => :log do
    opts = "#{RACKUP_FCGI_OPTS} --pid log/fcgi-#{FCGI_PORT}.pid --port #{FCGI_PORT}"
    sh "#{MAGLEV_HOME}/bin/rackup #{opts} config-fcgi.ru"
  end
end

namespace :client do
  desc "ab for count queries (default 5_000) on http://127.0.0.1:3333/app/magtag.css"
  task :ab, :count, :file do |t, args|
    args.with_defaults :count => 5_000, :file => 'log/ab.out'
    wait_for_TIME_WAIT_to_clear
    puts "\n\n==== ab with #{args[:count]} queries ===============\n\n"
    sh "curl http://localhost:3333/app/info | tee #{args[:file]}"
    sh "ab -n #{args[:count]} -c 10 http://127.0.0.1:3333/app/magtag.css | tee -a #{args[:file]}"
  end
end

# Waits until the number of sockets in TIME_WAIT is below threshold
def wait_for_TIME_WAIT_to_clear(threshold=20)
  while true
    count = `netstat -a inet -n|grep TIME_WAIT|wc -l`.to_i
    return if count < threshold
    puts "TIME_WAIT count: #{count}"
    sleep 1
  end
end

# Kill the pid contained in pid_file using signal (default 'TERM'), and
# remove pid_file if success.
def kill_from_pidfile(pid_file, signal='TERM')
  puts "====== kill_from_pidfile(#{pid_file}, #{signal})"
  begin
    raise "Couldn't read file: '#{pid_file}'" unless pid = File.readlines(pid_file)
    puts "kill_from_pidfile: kill -s #{signal} #{pid}"
    sh "kill -s #{signal} #{pid}"
    rm_f pid_file
    return true
  rescue
    puts "Failed on file '#{pid_file}'  pid '#{pid}' (pid_file not removed)"
  end
  false
end
