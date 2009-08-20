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

helpers do
  def nav_bar
    <<EOS
<h3>Sample Blog App running on Sinatra #{Sinatra::VERSION}</h3>
<h3>Nav Bar</h3>
<ul>
  <li><a href="/post/new">New Post</a></li>
  <li><a href="/posts">All Posts</a></li>
</ul>
EOS
  end
end

error do
  e = request.env['sinatra.error']
  "There was an error: #{e}"
end

get '/' do
  #erb :index
  nav_bar
end

# To display a form for creating a new post
get '/post/new' do
  <<EOS
#{nav_bar}
<form method="post" action="/post">
  Title: <input type="text" name="title" /><br />
  Text:  <textarea name="text" rows="10" cols="50"></textarea><br />
  <input type="submit" value="Create">
</form>
EOS
end

# REST create new post
post '/post' do
  post = Post.new(params)
  Post.add(post)
  redirect "/post/#{post.__id__}"
end

get '/post/:id' do
  post = Post.get(params[:id])
  stop [ 404, "Page not found (id: #{params[:id]})" ] unless post
  <<EOS
#{nav_bar}
<h3>Title: #{post.title}</h3>
<p>#{post.text}</p>
EOS
end

# List all posts
get '/posts' do
  posts = Post.all_posts
  s = nav_bar
  s << "<p>Here are the #{posts.size} blog posts</p><ul>"
  posts.each do |post|
    s << "<li><a href=\"/post/#{post.__id__}\">#{post.title}</a></li>"
  end
  s << "</ul>"
end

# __END__

# @@ layout
# <html>
#   <head> <title>Simple Blog</title> </head>
#   <body>
#     <div id="nav">
#       <ul>
#         <li><a href="/post/new">New Post</a></li>
#         <li><a href="/posts">All Posts</a></li>
#       </ul>
#     </div>
#     <div id="content">
#       <%= yield %>
#     </div>
#   </body>
# </html>

# @@ index

# <h3>Welcome to a simple blog</h3>
