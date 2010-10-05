#
#config.ru
#
#       \     --port 3333

require 'maglev/rack_txn_wrapper' if defined? Maglev
require 'empty_middleware.rb'
require 'magtag_app'

# use Maglev::TransactionWrapper
use EmptyMiddleware

MagTag.set :public, File.expand_path(File.dirname(__FILE__)) + '/public'
MagTag.set :sessions, true

run MagTag.new
