require 'ramaze'

class MainController < Ramaze::Controller
  engine :Amrita2

  def index
    @data = { :hello => "Hello, World!" }
    %q[
     <<html<
       <<body<
         <<%<
           <% 10.times do %>
             <span><%= $_[:hello] %></span>
           <% end %>
    ]
  end
end
