# config-fcgi.ru
#
# NOTE: This is broken, pending https://magtrac.gemstone.com/ticket/799
#
require 'magtag_app'
require 'empty_middleware'

use EmptyMiddleware

MagTag.set :public, File.expand_path(File.dirname(__FILE__)) + '/public'
MagTag.set :sessions, true

map '/app' do
  run MagTag.new
end
