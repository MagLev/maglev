require 'ramaze'

class MainController < Ramaze::Controller
  engine :Ezamar

  def index
    @hello = "Hello, World!"
    '<html><body><? 10.times do ?><span>#{@hello}</span><?r end ?></body></html>'
  end
end
