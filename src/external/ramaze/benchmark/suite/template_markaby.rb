require 'ramaze'

class MainController < Ramaze::Controller
  engine :Markaby
  helper :markaby

  def index
    @hello = "Hello, World!"
    mab { html { body { 10.times { span @hello } } } }
  end
end
