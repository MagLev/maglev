require File.dirname(__FILE__) + '/ramaze/lib/ramaze'
class MainController < Ramaze::Controller
  def index
    "Hello from ramaze..."
  end
end
Ramaze.start
