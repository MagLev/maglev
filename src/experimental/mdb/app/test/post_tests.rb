# MiniTest suite for the application's Model
require 'rubygems'
require 'minitest/spec'
require 'blog.rb'
# require 'mdb/database'
# require 'mdb/server'

# require 'helpers'

MiniTest::Unit.autorun

# DB_NAME = MDB::Test.db_name 'database_tests'

describe Post do
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
end

