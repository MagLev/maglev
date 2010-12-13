require 'sinatra/base'

# A simple Sinatra blob app using MagLev.  This file depends on the
# config.ru to add the MagLevTransactionWrapper into the rack stack.  The
# transaction wrapper wraps each HTTP request in a MagLev transaction.  The
# MagLev.commit_transaction call is done by the transaction wrapper which
# will then commit any new posts, tags or changes to persistent data.
class BlogApp < Sinatra::Base

  set :app_file, __FILE__ # Affects :views, :public and :root

  def initialize(*args)
    super
    @title = 'Simple MagLev Powered Blog'
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
    @tag = SimpleTag.get(params[:id])
    raise "Page not found for tag (id: #{params[:id]})" unless @tag
    erb :tag
  end

  get '/post/new' do
    erb :newpost
  end

  post '/post' do
    post = SimplePost.persistent_new(params)
    ObjectLogEntry.info("A Post", post).add_to_log
    params[:tags].split.each do |tag|
      t = SimpleTag.find_by_name(tag) || SimpleTag.persistent_new(tag)
      post.tag t
    end if params[:tags]
    redirect "/post/#{post.__id__}"
  end

  get '/post/:id' do
    @post = SimplePost.get(params[:id])
    raise "Page not found (id: #{params[:id]})" unless @post
    erb :post
  end

  get '/posts' do
    @posts = SimplePost.all
    erb :blog
  end

  get '/debug' do
    @options = options
    erb :debug
  end
end
