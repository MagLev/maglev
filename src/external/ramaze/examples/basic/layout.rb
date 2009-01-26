require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  map '/'
  layout :page

  def index
    @title = "Test"
    "<p>Hello, World!</p>"
  end

  def page
    %{
<html>
  <head>
    <title>examples/layout</title>
  </head>
  <body>
    <h1>#@title</h1>
    #@content
  </body>
</html>
    }
  end
end

Ramaze.start
