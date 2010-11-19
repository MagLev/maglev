# Run MagTag app without a transaction wrapper

require 'magtag_app'

MagTag.set :public, File.expand_path(File.dirname(File.dirname(__FILE__))) + '/public'
MagTag.set :sessions, true

map '/app' do
  run MagTag.new
end
