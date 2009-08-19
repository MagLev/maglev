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

# For to create a new post
get '/post/new' do
  <<EOS
<form method="post" action="/post">
Title:  <input type="text" name="title" /><br />
Text:   <input type="text" name="text" /><br />
  <input type="submit" value="Create">
</form>
#{nav_bar}
EOS
end

# REST create new post
post '/post' do
  "Create a new post...#{params[:title]}"
  post = Post.new(:title => params[:title], :text => params[:text])
  Post.add(post)
  redirect "/post/#{post.id}"
end

get '/post/:id' do
  post = Post.get(params[:id])
  stop [ 404, "Page not found (id: #{params[:id]})" ] unless post
  <<EOS
<h2>#{post.title}</h2>
<p>#{post.text}</p>
#{nav_bar}
EOS
end

# List all posts
get '/posts' do
  posts = Post.all_posts
  s = "<h3>Sample Blog App running on Sinatra #{Sinatra::VERSION}</h3>"
  s << "<p>Here are the #{posts.size} blog posts</p><ul>"
  posts.each do |p|
    s << "<li>[#{p.id}]: #{p.title}</li>" if p
  end
  s << "</ul>" << nav_bar
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
