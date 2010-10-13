namespace :tests do

  desc "Test performance of reading static file on Maglev; no txn wrapper."
  task :static, :count do |t, args|
    args.with_defaults(:count => '1')
    pids = []
    pids << fork_task('lighttpd:scgi', args[:count])
    pids << fork_task('scgi:maglev', args[:count], 'config/no_txn_wrapper.ru')

    puts "Waiting for startup..."
    sleep 7
    Rake::Task['client:ab'].invoke(5_000, "log/static-#{args[:count]}.out")

    kill_and_reap pids
  end


  desc "Test performance no lock txn wrapper"
  task :no_lock_txn do
    pids = []
    pids << fork_task('lighttpd:scgi')
    pids << fork_task('scgi:maglev', 'config/no_lock_txn_wrap.ru')

    puts "Waiting for startup..."
    sleep 7
    Rake::Task['client:ab'].invoke

    kill_and_reap pids
  end
end


# Kill the process group named by pids
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

# Fork off a rake task; set the pgrp for easy kills later
def fork_task(task_name, *args)
  puts "== fork_task(#{task_name}, #{args.inspect})"
  pid = fork { Process.setpgrp ; Rake::Task[task_name].invoke(*args) }
  puts "    pid: #{pid}"
  puts `ps -jp #{pid}`
  pid
end
