require 'ramaze'

class MainController < Ramaze::Controller
  engine :Tenjin

  def index
    @context = {:hello => "Hello, World!"}
    '<html><body><?rb 10.times do ?><span>${@hello}</span><?rb end ?></body></html>'
  end
end
