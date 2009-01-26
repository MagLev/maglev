require 'ramaze'

class MainController < Ramaze::Controller
  engine :None

  def index
    "Hello, World!"
  end
end