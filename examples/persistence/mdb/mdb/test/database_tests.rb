# MiniTest suite for the MDB::Database
# Fixtures are in mdb_fixtures.rb, loaded by ../Rakefile

require 'rubygems'
require 'minitest/spec'

require 'mdb/database'
require 'mdb/server'
require 'helpers'

MiniTest::Unit.autorun

DB_NAME = MDB::Test.db_name 'database_tests'

describe MDB::Database do
  before do
    MDB::Test.delete_test_dbs
    @db = MDB::Server.server.create(DB_NAME, ViewClass)
  end

  it 'executes the requested view (view is symbol or string): no view parameters' do
    @db.execute_view('view_42').must_equal 42
    @db.execute_view(:view_42).must_equal 42
  end

  it 'executes the requested view (view is symbol or string): one view parameter' do
    @db.execute_view("view_55_plus", 11).must_equal 66
    @db.execute_view(:view_55_plus,  12).must_equal 67
  end

  it 'executes the requested view (view is symbol or string): two view parameters' do
    # View 67 sums: 67 + count + ary.length
    @db.execute_view("view_67", 5, [:foo, :bar]).must_equal 74
    @db.execute_view(:view_67,  5, [:foo]).must_equal 73
  end

  it 'raises NoSuchView if there is no view of the given name' do
    proc { @db.execute_view(:not_a_view_name) }.must_raise MDB::Database::NoViewError
  end

  it 'it accepts symbols for set_view' do
    @db.execute_view(:view_42).must_equal 42 # Ensure old view
    @db.set_view(:ViewClass2)
    @db.execute_view(:view_42).must_equal 43 # Ensure new view
  end

  it 'it accepts strings for set_view' do
    @db.execute_view(:view_42).must_equal 42 # Ensure old view
    @db.set_view("ViewClass2")
    @db.execute_view(:view_42).must_equal 43 # Ensure new view
  end

  it 'it accepts classes for set_view' do
    @db.execute_view(:view_42).must_equal 42 # Ensure old view
    @db.set_view(ViewClass2)
    @db.execute_view(:view_42).must_equal 43 # Ensure new view
  end

  it 'it searches and finds a path given to set_view' do
    @db.execute_view(:view_42).must_equal 42 # Ensure old view
    @db.set_view("Foo::Bar::ViewClass3")
    @db.execute_view(:view_42).must_equal 44 # Ensure new view
  end

  it 'raises an exception if set_view called with bogus class, string or symbol' do
    @db.execute_view(:view_42).must_equal 42 # Ensure old view
    proc { @db.set_view(:BogusViewClass2) }.must_raise MDB::MDBError
    proc { @db.set_view("BogusViewClass2") }.must_raise MDB::MDBError
    proc { @db.set_view("Foo::Bar::BogusViewClass2") }.must_raise MDB::MDBError
  end

  it 'adds the document to the saved documents and can get it by id' do
    my_document = Object.new
    id = @db.add(my_document)
    @db.get(id).must_equal my_document
    # TODO: need to ensure my_document is committed
  end

  it 'add calls the model callback function' do
    ViewClass.reset_count
    ViewClass.count.must_equal 0
    @db.add(Object.new)
    ViewClass.count.must_equal 1
  end

  it 'iterates over the documents' do
    # When no documents, should be 0
    count = 0
    @db.each { |d| count += 1 }
    count.must_equal 0

    4.times { |i| @db.add(Object.new) }
    count = 0
    @db.each { |d| count += 1 }
    count.must_equal 4
  end
end
