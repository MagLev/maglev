#
#config.ru
#
#       \     --port 3333

require 'maglev/rack_txn_wrapper' if defined? Maglev
require 'empty_middleware.rb'
require 'magtag_app'


if false
  puts "=== Using Rack::CommonLogger"
  # use Rack::CommonLogger Logger.new('log/magtag.log')
  use Rack::CommonLogger
end

# use Maglev::TransactionWrapper if defined? Maglev
# use EmptyMiddleware

MagTag.set :public, File.expand_path(File.dirname(__FILE__)) + '/public'
MagTag.set :sessions, true
# MagTag.set :environment, :production    # taling to sinatra here
run MagTag.new
