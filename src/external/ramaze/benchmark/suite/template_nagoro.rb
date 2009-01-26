require 'ramaze'

class MainController < Ramaze::Controller
  engine :Nagoro

  def index
    @hello = "Hello, World!"
    '<html><body><span times=10>#{@hello}</span></body></html>'
  end
end
