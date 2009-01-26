require 'ramaze'

class MainController < Ramaze::Controller
  engine :Builder

  def index
    @hello = "Hello, World!"
    %q[
      xml.html {|h| h.body {|b| 10.times { b.span @hello } }}
    ]
  end
end
