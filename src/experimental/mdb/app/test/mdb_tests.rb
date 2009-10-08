# MiniTest suite for the REST MDB wrapper.
# This test run by MRI
require 'rubygems'
require 'minitest/spec'
require 'mdb'
require 'app_model'

MiniTest::Unit.autorun

DB_NAME = 'rest_database_tests'
DB_NAME_2 = 'rest_database_tests2'

SERVER  = 'http://localhost:4567'

describe MDB::RESTServer do
  before do
    @server = MDB::RESTServer.new SERVER
    [DB_NAME, DB_NAME_2].each do |name|
      @server.delete name if @server.key? name
    end
    @db = @server.create DB_NAME, AppModel
    @db.clear
  end

  it 'responds to key? properly' do
    @server.key?(DB_NAME).must_equal true
    @server.key?(DB_NAME_2).must_equal false
  end

  it 'creates and deletes databases on the server' do
    db = @server.create DB_NAME_2, AppModel
    db.wont_be_nil
    @server.delete(DB_NAME_2).must_equal true
    proc { @server.delete(DB_NAME_2) }.must_raise RuntimeError
  end
end

describe MDB::RESTDatabase do
  before do
    @server = MDB::RESTServer.new SERVER
    @server.delete DB_NAME if @server.key? DB_NAME
    @db = @server.create DB_NAME, AppModel
    @db.clear
  end

  it 'starts off empty' do
    @db.size.must_equal 0
    @db.list_ids.size.must_equal 0
  end

  it 'adds documents and can retrieve them' do
    # TODO: the roundtrip converts symbols to strings, so define this
    # hash with strings until we use a Mash or something
    # Also, Time.now became a string in the roundtrip...
    blog_post = { :title => 'a title', :text => 'some text' }
    id = @db.add(blog_post)
    id.wont_be_nil
    id.class.must_equal Fixnum

    copy = @db.get(id)
    copy.must_equal blog_post
  end

  it 'can round trip random data' do
    data = AppModel.new(6, 7)
    id = @db.add(data)
    id.wont_be_nil
    id.class.must_equal Fixnum

    copy = @db.get(id)
    copy.x.must_equal 6
    copy.y.must_equal 7
  end
end
