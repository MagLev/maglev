# config-fcgi.ru
#
# There seems to be a bug in Lighttpd 1.4.x with PATH_INFO and SCRIPT_NAME.
# I've included rack middleware to fix it.
#
require 'magtag_app'
require 'fix_lighttpd_fcgi'

use FixLighttpdFastCGI

MagTag.set :public, File.expand_path(File.dirname(__FILE__)) + '/public'
MagTag.set :sessions, true

map '/app' do
  run MagTag.new
end
