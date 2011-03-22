
require 'webtools_app'

WebToolsApp.disable :run
#WebToolsApp.set :public, File.expand_path(File.dirname(__FILE__)) + '/public'
WebToolsApp.set :root, File.dirname(__FILE__)

WebToolsApp.run!
