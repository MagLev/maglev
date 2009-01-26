require 'rubygems'
require 'ramaze'

require 'model/history'
require 'model/message'

class MainController < Ramaze::Controller
  HISTORY = History.new

  layout '/layout'

  def index
    if request.post?
      session[:nick] = request[:nick]
      redirect Rs(:chat)
    end
  end

  def chat
    redirect Rs(:/) unless session[:nick]
  end

  def say
    if nick = session[:nick] and text = request['text']
      HISTORY.write(nick, text)
    end
  end

  def listen
    respond HISTORY.to_html
  end

  [ "Hello, World!",
    "My name is manveru",
    "I welcome you to my realm",
    "The unique and most awesome examples/chat.rb!",
  ].each{|text| HISTORY.write('manveru', text) }
end

Ramaze.start :middleware => true, :adapter => :thin
