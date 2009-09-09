require 'rubygems'
require 'sinatra/base'
require 'txn_wrapper'

class BlogApp < Sinatra::Base

  # When running out of a classic top level Sinatra app, several options
  # are set.  We have to set them here if we want them in a rackable app.
  set :server, ["webrick"]   # Maglev currently only supports webrick
  set :environment, :development
  set :static, true                # Allow loading /style.css, etc.
  set :app_file, __FILE__          # Affects :views, :public and :root

  # use Rack::Lint
  # use Rack::Reloader   # sort-of works....

  use MagLevTransactionWrapper

  def initialize(*args)
    super
    @title = 'Simple Sinatra Blog'
    @nav_bar =  <<-EOS
        <ul class="menu">
          <li><a href="/posts">All Posts</a></li>
          <li><a href="/post/new">New Post</a></li>
        </ul>
    EOS
    @show_tags = true
  end

  error do
    e = request.env['sinatra.error']
    "There was an error: #{e}"
  end

  get '/' do
    redirect '/posts'
  end

  get '/tag/:id' do
    @tag = Tag.get(params[:id])
    raise "Page not found for tag (id: #{params[:id]})" unless @tag
    erb :tag
  end

  get '/post/new' do
    erb :newpost
  end

  post '/post' do
    post = Post.new(params)
    ObjectLogEntry.info("A Post", post).add_to_log
    params[:tags].split.each do |tag|
      t = Tag.find_by_name(tag) || Tag.new(tag)
      post.tag t
    end if params[:tags]
    redirect "/post/#{post.__id__}"
  end

  get '/post/:id' do
    @post = Post.get(params[:id])
    raise "Page not found (id: #{params[:id]})" unless @post
    erb :post
  end

  get '/posts' do
    @posts = Post.all
    erb :blog
  end

  get '/debug' do
    @options = options
    erb :debug
  end
end
