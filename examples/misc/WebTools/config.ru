require 'webtools_app'

WebToolsApp.disable :run
WebToolsApp.set :root, File.dirname(__FILE__)

run WebToolsApp.new
