# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

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
