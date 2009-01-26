require 'rubygems'
require 'ramaze'

# you can access it now with http://localhost:7000/
# This should output
# Hello, World!
# in your browser

class MainController < Ramaze::Controller
  def index
    "Hello, World!"
  end
end

Ramaze.start
