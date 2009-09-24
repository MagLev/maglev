require 'rubygems'
require 'sinatra'
require 'json'
require 'httpclient'

# The BlogApp implements the View and Controller parts of the MVC
# pattern.  This app receives end user http requests, invokes a model
# method (through a "view"), and then renders the appropriate view from the
# model data.
#
# The data returned by the model is JSON.
#
# Run by MRI
# Listens on port 3333
# Contacts MDB on port 4567
#
class BlogApp < Sinatra::Base
  set :app_file, File.dirname(__FILE__)
  set :static, true

#  require '../sinatra/log_headers.rb'
#  use LogHeaders

  SERVER_URI = 'http://localhost:4567'
  SERVER = HTTPClient.new

  def data_for(path)
    begin
      SERVER.get_content(SERVER_URI + path)
    rescue HTTPClient::BadResponseError => bre
      halt 404, "get_content: Error doing GET from database: #{bre.res.content}"
    end
  end

  def put_to(data, path)
    begin
      SERVER.put(SERVER_URI + path, data)
    rescue HTTPClient::BadResponseError => bre
      halt 404, "put_to: Error doing PUT to database: #{bre.res.content}"
    end
  end

  def initialize(*args)
    super
    @title = "MRI Blog Using MagLev DB"
    @nav_bar =  <<-EOS
        <ul class="menu">
          <li><a href="/">Home</a></li>
          <li><a href="/new/post">New Post</a></li>
        </ul>
    EOS
  end

  get '/' do
    json = data_for('/post/recent')
    @posts = JSON.parse(json)
    erb :home
  end

#   get '/post/:id' do
#     json = data_for("/post/#{params[:id]}")
#     @post = JSON.parse(json)
#     erb :post
#   end

#   # Display a form to create a new blog post
#   get '/new/post' do
#     erb :newpost
#   end

#   # Submitting a /new/post form goes here.
#   post '/post' do
#     new_params = {
#       :title => params[:title],
#       :text => params[:text],
#       :tags => params[:tags]
#     }
#     response = put_to(new_params, '/post')

#     case response.status
#     when 200..299
#       json = response.content
#       post = JSON.parse json
#       redirect "/post/#{post["id"]}"  # need to add Mash...
#     when 300..399
#       halt 404, "Can't redirect to server:  Server response #{response.status}"
#     else
#       halt response.status, "Server response #{response.status}"
#     end
#   end
end

BlogApp.run!   :host  => 'localhost', :port => 3333
