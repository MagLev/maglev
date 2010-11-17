# Tasks to run various configurations of Ruby and HTTP servers.
# Assumes the following:
#
# 1. MagLev is installed in $MAGLEV_HOME
# 2. RVM is installed in $HOME/.rvm
# 3. RVM has 1.8.7 and 1.9.2 versions of MRI

namespace :webrick do
  # TODO: DRY :webrick and :scgi....there is much similar
  desc "run MRI via rvm; uses webrick"
  task :mri, :rackup_file, :version do |t, args|
    args.with_defaults :rackup_file => 'config/no_txn_wrapper.ru', :version => '1.8.7'
    sh "source $HOME/.rvm/scripts/rvm ; rvm #{args[:version]} && rackup #{RACKUP_OPTS} --port 3333 #{args[:rackup_file]}"
  end

  desc "run maglev httpd"
  task :maglev, :rackup_file, :port do |t, args|
    bail_if_rvm_hosing_environment
    args.with_defaults :rackup_file => 'config/no_txn_wrapper.ru', :port => 3333
    sh "$MAGLEV_HOME/bin/rackup #{RACKUP_OPTS} --port #{args[:port]} #{args[:rackup_file]}"
  end
end
