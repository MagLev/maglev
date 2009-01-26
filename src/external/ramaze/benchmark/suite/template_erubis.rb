require 'ramaze'

class MainController < Ramaze::Controller
  engine :Erubis

  def index
    @hello = "Hello, World!"
    '<html><body><% 10.times do %><span><%= @hello %></span><% end %></body></html>'
  end
end
