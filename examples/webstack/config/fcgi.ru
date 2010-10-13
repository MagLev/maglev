# config-fcgi.ru
#
# NOTE: This is broken, pending https://magtrac.gemstone.com/ticket/799
#
require 'fcgi'

require 'maglev/rack_txn_wrapper' if defined? Maglev
require 'magtag_app'

use Maglev::TransactionWrapper if defined? Maglev

MagTag.set :public, File.expand_path(File.dirname(__FILE__)) + '/public'
MagTag.set :sessions, true

map '/app' do
  run MagTag.new
end
