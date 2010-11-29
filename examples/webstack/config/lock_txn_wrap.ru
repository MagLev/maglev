# lock_txn.ru
#
# Configures a Locking transaction wrapper

require 'locking_txn_wrapper' if defined? Maglev
require 'magtag_app'

use LockingTransactionWrapper if defined? Maglev

MagTag.set :public, File.expand_path(File.dirname(File.dirname(__FILE__))) + '/public'
MagTag.set :sessions, true

map '/app' do
  run MagTag.new
end
