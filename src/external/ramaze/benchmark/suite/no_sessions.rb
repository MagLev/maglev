require 'ramaze'

class MainController < Ramaze::Controller
  def index
    "Hello, World!"
  end
end

Ramaze::Log.loggers = []
Ramaze::Global.sessions = false
