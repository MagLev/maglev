require 'rubygems'
require 'ramaze'
set :app_file,__FILE__
class MainController < Ramaze::Controller
      def index
        'Hello, World!'
      end
end
Ramaze.start
