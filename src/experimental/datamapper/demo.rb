require 'rubygems'
require 'dm-core'

DataMapper::Logger.new($stdout, :debug)

DataMapper.setup(:default, :adapter => :in_memory)
# DataMapper.setup(:default, 'sqlite3::memory:')

class Post
  include DataMapper::Resource

  has n, :comments
  has n, :categorizations
  has n, :categories, :through => :categorizations

  property :id,         Serial
  property :title,      String
  property :body,       Text
  property :created_at, DateTime
end

class Comment
  include DataMapper::Resource

  belongs_to :post

  property :id,         Serial
  property :posted_by,  String
  property :email,      String
  property :url,        String
  property :body,       Text
end

class Category
  include DataMapper::Resource

  has n, :categorizations
  has n, :posts, :through => :categorizations

  property :id,         Serial
  property :name,       String
end

class Categorization
  include DataMapper::Resource

  property :id,         Serial
  property :created_at, DateTime

  belongs_to :category
  belongs_to :post
end

# In memory adapter appears not to support/need auto_migrate!
# DataMapper.auto_migrate!

post1 = Post.new(:title => 'Post number 1',
                 :body => 'Body of Post number 1',
                 :created_at => Time.now)

p Post.all
p post1
p post1.save
p Post.all

