require 'rubygems'
require 'sinatra'
require 'txn_wrapper'

use MagLevTransactionWrapper

# Workaround for issue automatically starting app.
# See comments in README file
configure(:development) do
  set :run, true
end

Exception.install_debug_block do |e|
  case e
  when NoMethodError
    puts "---- #{e} -- (#{e.class})  selector: #{e.selector} args: #{e.args} env: #{e.envid}"
    selector = e.selector.to_s
#    nil.pause if selector == "id"
#    nil.pause if selector == "render_erb"
  else
    #puts "---- #{e} -- (#{e.class})"
  end
end

error do
  e = request.env['sinatra.error']
  "There was an error: #{e}"
end

get '/' do
  redirect '/posts'
end

get '/post/new' do
  erb :newpost
end

post '/post' do
  post = Post.new(params)
  Post.add(post)
  redirect "/post/#{post.__id__}"
end

get '/post/:id' do
  @post = Post.get(params[:id])
  stop [ 404, "Page not found (id: #{params[:id]})" ] unless @post
  erb :post
end

get '/posts' do
  @posts = Post.all_posts
  erb :index
end

__END__

@@ layout
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <title>Simple Sinatra Blog</title>
    <meta http-equiv="Content-Type" content="text/html;  charset=iso-8859-1" />
    <link rel="stylesheet" type="text/css" href="/style.css" media="all" />
  </head>
  <body>
    <div id="container">
      <div id="header">
        <h1>Simple Sinatra Blog</h1>
      </div>
      <div id="navigation">
        <ul class="menu">
          <li><a href="/post/new">New Post</a></li>
          <li><a href="/posts">All Posts</a></li>
        </ul>
      </div>
      <div id="content">
        <div class="left"><%= yield %></div>
        <div class="right">
          <h4>Recent Posts</h4>
          <p>TBD</p>
          <h4>Tags</h4>
          <p>TBD</p>
          <h4>About</h4>
          <p>This is a simple sinatra blog running on MagLev</p>
          <h4>Credit</h4>
          <p>The layout is <a href="http://css4free.com/wp-content/uploads/templates/zineprint/">zineprint</a>, Designed by
             <a href="http://rizzle.us.to">Rizzle Studios</a>.
        </div>
      </div>
      <div id="footer">
        Simple Sinatra Blog running on <a href="http://maglev.gemstone.com">MagLev</a>
      </div>
    </div>
  </body>
</html>

@@ index
<h3>Welcome to a simple blog</h3>
<ul>
<% @posts.each do |post| %>
  <li><a href="/post/<%= post.__id__ %>"> <%= post.title %></a></li>
<% end %>
</ul>

@@ newpost
<form method="post" action="/post">
  Title: <input type="text" name="title" /><br />
  Text:  <textarea name="text" rows="10" cols="50"></textarea><br />
  <input type="submit" value="Create">
</form>

@@ post
<h3><%= @post.title %></h3>
<p><%= @post.text %></p>
