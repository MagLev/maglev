
# namespace :tests do

#   desc "Test performance of reading static file on Maglev; no txn wrapper."
#   task :static, :count do |t, args|
#     args.with_defaults(:count => '1')
#     pids = []
#     pids << fork_task('lighttpd:scgi', args[:count])
#     pids << fork_task('scgi:maglev', args[:count], 'config/no_txn_wrapper.ru')

#     puts "Waiting for startup..."
#     sleep 7
#     Rake::Task['client:ab'].invoke(5_000, "log/static-#{args[:count]}.out")

#     kill_and_reap pids
#   end


#   desc "Test performance no lock txn wrapper"
#   task :no_lock_txn do
#     pids = []
#     pids << fork_task('lighttpd:scgi')
#     pids << fork_task('scgi:maglev', 'config/no_lock_txn_wrap.ru')

#     puts "Waiting for startup..."
#     sleep 7
#     Rake::Task['client:ab'].invoke

#     kill_and_reap pids
#   end

#   namespace :nginx do
#     desc "Test nginx in front of four MagLev + WEBrick instances, static page."
#     task :static, :count do |t, args|
#       args.with_defaults(:count => '4')
#       pids = []

#       port = 3000
#       args[:count].to_i.times do
#         pids << fork_task('webrick:maglev', 'config/no_txn_wrapper.ru', port)
#         port += 1
#       end

#       puts "Waiting for startup..."
#       sleep 15

#       puts "Starting nginx..."
#       fork_task('nginx:proxy',   args[:count])
#       sleep 5

#       Rake::Task['client:ab'].invoke(5_000, "log/nginx-static-#{args[:count]}.out")

#       kill_and_reap pids
#       Rake::Task['nginx:kill!'].invoke
#     end
#   end
# end
