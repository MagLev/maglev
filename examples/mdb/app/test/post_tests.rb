# MiniTest suite for the application's Model
require 'rubygems'
require 'minitest/spec'
require 'blog.rb'

MiniTest::Unit.autorun

SECONDS_PER_DAY = 60 * 60 * 24

describe Post do
  before do
    @posts = Hash.new
    start = Time.now - (60 * 60 * 24 * 30)
    @even = Tag.new("even")
    @odd  = Tag.new("odd")
    @all = Tag.new("all")
    10.times do |i|
      tags = (i % 2 == 0) ? [Tag.new('maglev'), @even, @all ] : [Tag.new('blog'), @odd, @all ]
      @posts[i] = Post.new({
        :title => "Title #{i}",
        :text => "Text #{i}",
        :timestamp => start + SECONDS_PER_DAY * i, # create a time-stamp i days ago
        :tags => tags })
      Post.document_added(i, @posts[i])  # so that @recent_posts is updated correctly.
    end
  end

  it 'creates new instances from a hash' do
    params = {
      :title => 'The title',
      :text => 'The text',
      :timestamp => Time.now,
      :tags => ['maglev', 'blog']
    }

    post = Post.new(params)
    [:title, :text, :timestamp, :tags].each do |attr|
      post.send(attr).must_equal params[attr]
    end
  end

  it 'auto-populates @timestamp and @tags if not passed to initialize' do
    post = Post.new(:title => 'The title', :text => 'The text')
    post.wont_be_nil
    post.tags.must_equal []
    post.timestamp.wont_be_nil
  end

  it 'returns the recent posts' do
    recent_posts = Post.recent(@posts)
    recent_posts.size.must_equal 5
    recent_posts.each { |p| p.class.must_equal Post}
  end

  it 'knows what tags it is tagged with' do
    @posts.each do |_,post|
      post.tagged_with?('all').must_equal true
      post.tagged_with?(:all).must_equal true
      post.tagged_with?('maglevitate').must_equal false
    end
  end
end

describe "Post view methods" do
  before do
    @posts = Hash.new
    start = Time.now - (60 * 60 * 24 * 30)
    @even = Tag.new("even")
    @odd  = Tag.new("odd")
    @all = Tag.new("all")
    10.times do |i|
      tags = (i % 2 == 0) ? [Tag.new('maglev'), @even, @all ] : [Tag.new('blog'), @odd, @all ]
      @posts[i] = Post.new({
        :title => "Title #{i}",
        :text => "Text #{i}",
        :timestamp => start + SECONDS_PER_DAY * i, # create a time-stamp i days ago
        :tags => tags })
      Post.document_added(i, @posts[i])  # so that @recent_posts is updated correctly.
    end
  end

  it 'finds posts tagged by name' do
    Post.tagged_with(@posts, "even").size.must_equal 5
    Post.tagged_with(@posts, "odd").size.must_equal 5
    Post.tagged_with(@posts, "maglev").size.must_equal 5
    Post.tagged_with(@posts, "blog").size.must_equal 5
    Post.tagged_with(@posts, "all").size.must_equal 10
  end

  it 'maintains the recent posts queue' do
    recent = Post.recent @posts
    recent[0].title.must_equal "Title 9"
    recent[4].title.must_equal "Title 5"

    recent.must_equal(Post.recent_fast @posts)
  end
end
