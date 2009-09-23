# MiniTest suite for the MDB::Server
require 'rubygems'
require 'minitest/spec'
require 'mdb/server'

MiniTest::Unit.autorun

# Create a prefix for databases, so we can remove all test DBs w/o
# messing with other DBs served by the same MagLev repository
TEST_DB_PREFIX = "minitest_db"
TEST_DB_REGEXP = /^#{TEST_DB_PREFIX}/

describe "MDB::Server class side" do
  before do
    MDB::Server.db_names.each do |name|
      if name =~ TEST_DB_REGEXP
        begin
          MDB::Server.delete name
        rescue MDB::Server::DatabaseNotFound
          $stderr.puts "--- unexpected #{e} in before()"
        end
      end
    end
  end

  it "raises DatabaseNotFound when deleteing a non-existent db" do
    key = TEST_DB_PREFIX + "bogus"
    MDB::Server[key].must_be_nil
    proc { MDB::Server.delete(key) }.must_raise MDB::Server::DatabaseNotFound
  end

  it "raises DatabaseExists when creating a pre-existent db" do
    key = TEST_DB_PREFIX + "X"
    MDB::Server[key].must_be_nil
    MDB::Server.create(key)
    MDB::Server[key].wont_be_nil
    proc { MDB::Server.create(key) }.must_raise MDB::Server::DatabaseExists
  end

  it "creates, deletes and accesses databases" do
    key = TEST_DB_PREFIX + "a_db"
    MDB::Server[key].must_be_nil

    MDB::Server.create(key)
    MDB::Server[key].wont_be_nil

    MDB::Server.delete(key)
    MDB::Server[key].must_be_nil
  end

  it "lists the databases it has" do
    MDB::Server.db_names.grep(TEST_DB_PREFIX).must_be_empty
    db_names = %w(foo bar quux)
    db_names.each do |name|
      full_name = (TEST_DB_PREFIX + name).to_sym
      MDB::Server.create(full_name)
      full_name.must_include MDB::Server.db_names
    end
  end
end

