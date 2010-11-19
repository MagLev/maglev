# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

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
    args.with_defaults :rackup_file => 'config/no_txn_wrapper.ru', :version => '1.8.7'
    sh "source $HOME/.rvm/scripts/rvm ; rvm #{args[:version]} && rackup #{RACKUP_SCGI_OPTS} #{args[:rackup_file]}"
  end

end
