require 'ramaze'
require 'ramaze/gestalt'

class MainController < Ramaze::Controller
  helper :gestalt

  def index
    build do
      html do
        head do
          title "Welcome to Gestalt"
        end

        body do
          h1 "Gestalt reports for duty"

          p "Feel free to take a look at my source.",
            "That's what open source is for anyway."

          a(:href => 'http://ramaze.net'){ 'ramaze.net' }
        end
      end
    end
  end
end

Ramaze.start :adapter => :mongrel
