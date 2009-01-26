# Ramaze version of the merb tutorial
# http://www.socialface.com/slapp/

require 'rubygems'
require 'ramaze'
require 'sequel'

DB = Sequel.sqlite :logger => Ramaze::Log

class Post < Sequel::Model
  set_schema{ primary_key :id; time :created_at; text :text }
  before_create{ self.created_at ||= Time.now }
  create_table
end

%w[Ramaze Rocks].abbrev.keys.sort.each{|text| Post.create(:text => text) }

class MainController < Ramaze::Controller
  def index
    @posts = Post.order(:created_at.desc).first(10)
%q(<h1>Welcome to Rapp</h1>
<h2>A minimal chat wall</h2>
<p>Recent Posts:</p>
<div id="posts" class="container">
  <?r @posts.each do |post| ?>
    <div id="post-#{post.id}" class="post">
      <p class="text">#{post.text}</p>
      <p class="created">#{post.created_at}</p>
    </div>
  <?r end ?>
</div>
<p>Post Something:</p>
<form action="#{Rs(:create)}" method="POST">
  <input type="text" size="40" name="text" />
  <input type="submit" value="Post Message!" />
</form>)
  end

  def create
    Post.create(:text => request[:text])
    redirect Rs(:/)
  end
end

Ramaze.start
