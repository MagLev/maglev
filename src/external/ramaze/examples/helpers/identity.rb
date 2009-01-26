require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  helper :identity

  def index
    if session[:openid_identity]
      %{<h1>#{flash[:success]}</h1>
        <p>You are logged in as #{session[:openid_identity]}</p>}
    else
      openid_login_form
    end
  end
end

Ramaze::Log.loggers.each{|l| l.log_levels << :dev }
Ramaze.start :adapter => :mongrel
