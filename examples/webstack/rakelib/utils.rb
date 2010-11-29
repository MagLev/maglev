# Utility methods used by several of the rakefiles.

# Start MagLev VMs via rackup.  Each VM is started with rackup using the
# +ru+ config file.  There will be a pid file and an output file for each
# process started.  The pid files can be used to kill the processes later.
#
# @param [String] opts   options to pass to rackup (e.g., "--server SCGI")
# @param [String] ru     the rackup .ru file to use
# @param [Fixnum] count  the number of servers to start (default 4)
# @param [String] prefix the name to use for pid files and output files (default "app")
# @param [Fixnum] start_port the starting port number (default 3000)
#
# @return nil
def rackup_on_ports(opts, ru, prefix='app', count=4, start_port=3000)
  listeners = pids_on_ports(start_port, count)
  raise "processes already listening on target ports: pids: #{listeners.inspect}. Run: rake vm:kill!" unless listeners.empty?

  count.to_i.times do |i|
    puts "--- Running process #{i}"
    port = start_port + i
    cmd = "#{MAGLEV_HOME}/bin/rackup #{opts} --pid log/#{prefix}-#{port}.pid --port #{port} #{ru}"
    out = "log/#{prefix}-#{i}.out"
    sh "echo  #{cmd} >  #{out}"        # zeros output and lets us know command
    sh "nohup #{cmd} >> #{out} 2>&1 &"
    sleep 1
  end
end

# Returns the pids of servers listening on ports 3000-3003
#
# @return [Array of Fixnum] an array of process ids: ['1234', '456']
def pids_on_ports(first=3000, count=4)
  last = first + count - 1
  `lsof -Fp -i tcp:#{first}-#{last}`.split("\n").map { |el| el[1,el.length].to_i }
end

# Kill the process group named by pids
#
# @param [Array of String] pids   the process ids to kill.
def kill_and_reap(pids)
  puts "Sending signals..."
  sig = :KILL
  pids.each do |pid|
    puts "== kill #{pid} with #{sig}"
    Process.kill(sig, -1 * pid.to_i)
  end

  pids.each do |pid|
    puts "=== Waiting for: #{pid} #{Process.waitpid2(pid)}"
  end
end

# Invoke a Rake task in a separate process.  The process group of the
# process is set to the pid of the forked child, so that kill_and_reap can
# send signals to all jobs started by the task.
#
# Example: fork_task(:foo, :a, 10) will fork a child process, set the
# process group and then do:
#
#   Rake::Task[:foo].invoke(:a, 10)
#
# @param [String] task_name
# @param [Array of String] *args  Arguments passed to the invoked task
#
# @return [String] the pid of the forked task (should also be the same as
#                  the process group of the forked task.).
def fork_task(task_name, *args)
  puts "== fork_task(#{task_name}, #{args.inspect})"
  pid = fork { Process.setpgrp ; Rake::Task[task_name].invoke(*args) }
  puts "    pid: #{pid}"
  puts `ps -jp #{pid}`
  pid
end


# RVM sets GEM_PATH, and I don't run maglev under RVM, so I detect and bail
# if I accidentally run in an rvm-ified shell.
def bail_if_rvm_hosing_environment
  if ENV['rvm_path']
    puts "ERROR: Running maglev with RVM environment"
    exit 1
  end
end

# Waits until the number of sockets in TIME_WAIT is below threshold
#
# @param [Fixnum] threshold the maximum number of sockets in TIME_WAIT to
# allow
def wait_for_TIME_WAIT_to_clear(threshold=20)
  while true
    count = `netstat -a inet -n|grep TIME_WAIT|wc -l`.to_i
    return if count < threshold
    puts "TIME_WAIT count: #{count}"
    sleep 1
  end
end

# Kill the pid contained in pid_file using signal (default 'TERM'), and
# remove pid_file if success.  Also sends SIGCONT to wake up stopped
# processes.
#
# @param [String] pid_file name of the pid file (contents should be just the pid).
# @param [String] signal name of the signal to use (default is 'TERM')
#
# @return [Boolean] returns true iff the pid file was found and the signal was sent.
def kill_from_pidfile(pid_file, signal='TERM')
  puts "====== kill_from_pidfile(#{pid_file}, #{signal})"
  begin
    raise "Couldn't read file: '#{pid_file}'" unless pid = File.readlines(pid_file)
    puts "kill_from_pidfile: kill -s #{signal} #{pid}"
    sh "kill -s #{signal} #{pid}"
    sh "kill -s CONT #{pid}"  do |ok,res|
      # send sig continue in case it is asleep; ignore errors
    end
    rm_f pid_file
    return true
  rescue Exception => ex
    puts "Failed on file '#{pid_file}'  pid '#{pid}' (pid_file not removed): #{ex.inspect}"
  end
  false
end
