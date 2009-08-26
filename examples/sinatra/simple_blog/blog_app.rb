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
  erb :index
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
          <li><a href="/objectlog">Object Log</a></li>
        </ul>
      </div>
      <div id="content">
        <div class="left"><%= yield %></div>
        <div class="right">
          <h4>Recent Posts</h4>
          <p>TBD</p>
          <h4>Tags</h4>
          <ul>
            <% Tag.all.each do |tag| %>
            <li><a href="/tag/<%= tag.__id__ %>"><%= tag.name %></a></li>
            <% end %>
          </ul>
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
  <ul>
    <li>Title: <input type="text" name="title" /> </li>
    <li>Tags:  <input type="text" name="tags" /> </li>
    <li>Text:  <textarea name="text" rows="10" cols="50"></textarea></li>
    <li><input type="submit" value="Create"></li>
  </ul>
</form>

@@ post
<h3><%= @post.title %></h3>
<p> Tags: <%= @post.tags.map{ |t| t.name }.join(" ") %> </p>
<p><%= @post.text %></p>

@@ tag
<h3>Posts tagged with <%= @tag.name %></h3>
<ul>
  <% @tag.each do |post| %>
  <li><a href="/post/<%= post.__id__ %>"> <%= post.title %></a></li>
  <% end %>
</ul>


@@ objectlog
<table>
  <tr><th>#</th><th>Tag</th><th>Label</th><th>Object</th><th>Timestamp</th></tr>
<% @objectlog.reverse_each_with_index do |entry,i| %>
  <tr>
    <td><%= i %></td>
    <td><%= entry.tagged? ? "X" : "" %></td>
    <td><a href="/entry/<%= i %>"> <%= entry.label %></a></td>
    <td><%= entry.object %></td>
    <td><%= entry.timestamp %></td>
  </tr>
<% end %>
</table>

@@ objectdetail
<h3>Object Detail </h3>
<table>
  <tr><th>Attribute</th><th>Value</th></tr>
  <tr><td>Class</td><td><a href="/object/<%= @object.class.object_id %>"><%= @object.class %></a></td></tr>
  <tr><td>Oop</td><td> <%= @object.object_id %></td></tr>
  <% @object.instance_variables.each do |var|
       ivar = @object.instance_variable_get(var)
  %>
    <tr>
      <td><%= var %></td>
      <td><a href="/object/<%= ivar.object_id %>"><%= ivar.inspect %></td>
    </tr>
  <% end %>
</table>

