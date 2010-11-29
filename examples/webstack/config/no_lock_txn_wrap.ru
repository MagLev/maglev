# no_lock_txn.ru
#
# Configures a Non-Locking transaction wrapper

require 'no_lock_txn_wrapper' if defined? Maglev
require 'magtag_app'

use TransactionWrapper if defined? Maglev

MagTag.set :public, File.expand_path(File.dirname(File.dirname(__FILE__))) + '/public'
MagTag.set :sessions, true

map '/app' do
  run MagTag.new
end
