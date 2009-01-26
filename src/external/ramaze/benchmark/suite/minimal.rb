require 'ramaze'

class MainController < Ramaze::Controller
  engine :None

  def index
    "Hello, World!"
  end
end

Ramaze::Global.sourcereload = false
Ramaze::Global.sessions = false
Ramaze::Log.loggers = []
