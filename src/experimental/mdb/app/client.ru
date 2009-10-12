$:.unshift File.dirname(__FILE__) + '/lib'
$:.unshift File.dirname(__FILE__) + '/../mdb/lib'

require 'client_app'

BlogApp.run! :host => 'localhost',
             :port => 3333
