# MiniTest suite for the REST MDB wrapper.
# This test run by MRI
require 'rubygems'
require 'minitest/spec'
require 'mdb'
require 'app_model'

MiniTest::Unit.autorun

DB_NAME = 'rest_database_tests'

describe MDB::RESTDatabase do
  before do
    @db = MDB::RESTDatabase.new('http://localhost:4567', DB_NAME)
  end

#   it 'starts off empty' do
#     @db.size.must_equal 0
#     @db.list_ids.size.must_equal 0
#   end

  it 'adds documents and can retrieve them' do
    blog_post = { :title => 'a title', :ts => Time.now, :text => 'some text' }
    id = @db.add(blog_post)
    STDERR.puts "-- #{self}: id: #{id.inspect} (#{id.class})"
    id.wont_be_nil
    id.class.must_equal Fixnum

    copy = @db.get(id)
    copy.must_equal blog_post
  end
end
