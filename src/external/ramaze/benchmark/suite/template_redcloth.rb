require 'ramaze'

class MainController < Ramaze::Controller
  engine :RedCloth
  layout :layout

  def index
    @hello = "Hello, World!"
    '<% 10.times do %> %<%= @hello %>% <% end %>'
  end

  def layout
    '<html><body><%= @content %></body></html>'
  end
end
