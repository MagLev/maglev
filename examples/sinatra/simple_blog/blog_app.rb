require 'rubygems'
require 'sinatra'
require 'txn_wrapper'
require 'maglev/objectlog'

use MagLevTransactionWrapper

Exception.install_debug_block do |e|
  name = e.class.name
  unless ['RubyBreakException', 'RubyThrowException'].include? name
    ObjectLogEntry.debug(name, e.backtrace).add_to_log
  end
end


# Workaround for issue automatically starting app.
# See comments in README file
configure(:development) do
  set :run, true
  App = {
    :title => 'Simple Sinatra Blog',
    :show_tags => true,
    :nav_bar => <<-EOS
        <ul class="menu">
          <li><a href="/post/new">New Post</a></li>
          <li><a href="/posts">All Posts</a></li>
          <li><a href="/objectlog">Object Log</a></li>
        </ul>
    EOS
  }
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
  stop [ 404, "Page not found for tag (id: #{params[:id]})" ] unless @tag
  erb :tag
end

get '/post/new' do
  erb :newpost
end

post '/post' do
  post = Post.new(params)
  params[:tags].split.each do |tag|
    t = Tag.find_by_name(tag) || Tag.new(tag)
    post.tag t
  end if params[:tags]
  redirect "/post/#{post.__id__}"
end

get '/post/:id' do
  @post = Post.get(params[:id])
  stop [ 404, "Page not found (id: #{params[:id]})" ] unless @post
  erb :post
end

get '/posts' do
  @posts = Post.all
  erb :blog
end


#################################################
# Object Log Support
#################################################
get '/objectlog' do
  @objectlog = ObjectLogEntry.object_log
  erb :objectlog
end

get '/entry/:id' do
  index = params[:id].to_i
  @object = ObjectLogEntry.object_log[index]
  stop [ 404, "Can't find Object Log Entry for index: #{index}" ] unless @object
  erb :objectdetail
end

get '/object/:id' do
  oop = params[:id].to_i
  @object = Object._object_for_oop(oop)
  stop [ 404, "Can't find object with oop #{oop}" ] unless @object
  erb :objectdetail
end

