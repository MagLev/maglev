# MiniTest suite for the Database.rb code
require 'rubygems'
require 'minitest/spec'

MiniTest::Unit.autorun

TEST_DB_PREFIX = "minitest_db"
TEST_DB_REGEXP = /^#{TEST_DB_PREFIX}/

describe "MDB::Database class side" do
  before do
    MDB::Database.db_names.each do |name|
      if name =~ TEST_DB_REGEXP
        begin
          MDB::Database.delete name
        rescue MDB::Database::DatabaseNotFound
          $stderr.puts "--- unexpected #{e} in before()"
        end
      end
    end
  end

  it "raises DatabaseNotFound when deleteing a non-existent db" do
    key = TEST_DB_PREFIX + "bogus"
    MDB::Database[key].must_be_nil
    proc { MDB::Database.delete(key) }.must_raise MDB::Database::DatabaseNotFound
  end

  it "raises DatabaseExists when creating a pre-existent db" do
    key = TEST_DB_PREFIX + "X"
    MDB::Database[key].must_be_nil
    MDB::Database.create(key)
    MDB::Database[key].wont_be_nil
    proc { MDB::Database.create(key) }.must_raise MDB::Database::DatabaseExists
  end

  it "creates, deletes and accesses databases" do
    key = TEST_DB_PREFIX + "a_db"
    MDB::Database[key].must_be_nil

    MDB::Database.create(key)
    MDB::Database[key].wont_be_nil

    MDB::Database.delete(key)
    MDB::Database[key].must_be_nil
  end

  it "lists the databases it has" do
    MDB::Database.db_names.grep(TEST_DB_PREFIX).must_be_empty
    db_names = %w(foo bar quux)
    db_names.each do |name|
      full_name = (TEST_DB_PREFIX + name).to_sym
      MDB::Database.create(full_name)
      full_name.must_include MDB::Database.db_names
    end
  end
end

