$:.unshift File.dirname(__FILE__) + '/lib'

require 'demo'

Demo.run! :host => 'localhost', :port => 3333
