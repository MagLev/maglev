require 'ramaze'

class MainController < Ramaze::Controller
  engine :Haml

  def index
    @hello = "Hello, World!"
    %{
%html
  %body
    - 10.times do
      %span= @hello
    }
  end
end
