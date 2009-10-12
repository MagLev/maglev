# MiniTest suite for the application's Model
require 'rubygems'
require 'minitest/spec'
require 'blog.rb'

MiniTest::Unit.autorun

SECONDS_PER_DAY = 60 * 60 * 24

describe Post do
  before do
    @data = Hash.new
    now = Time.now
    10.times do |i|
      @data[i] = Post.new({
        :title => "Title #{i}",
        :text => "Text #{i}",
        :timestamp => now -  SECONDS_PER_DAY * i, # create a time-stamp i days ago
        :tags => [Tag.new('maglev'), Tag.new('blog')]})
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
    recent_posts = Post.recent(@data)
    recent_posts.size.must_equal 5
    recent_posts.each { |p| p.class.must_equal Post}
  end

  it 'knows what tags it is tagged with' do
    @data.each do |_,post|
      post.tagged_with?('maglev').must_equal true
      post.tagged_with?('maglevitate').must_equal false
    end
  end
end

