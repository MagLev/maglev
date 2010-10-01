#
#config.ru 
#
#\ --port 3333

require 'magtag_app'

MagTag.set :public, File.expand_path(File.dirname(__FILE__)) + '/public'
MagTag.set :sessions, true

run MagTag.new
