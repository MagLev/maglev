#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'
require 'ramaze/store/default'

describe "Store::Default" do
  db = 'db.yaml'

  after do
    FileUtils.rm db
  end

  def add hash = {}
    Books.merge!(hash)
  end

  it "should initialize Store::Default" do
    Books = Ramaze::Store::Default.new(db)
    Books.db.should.is_a?(YAML::Store)
  end

  it "should store and retrieve key/value" do
    add 'Pickaxe' => 'good book'
    Books['Pickaxe'].should == 'good book'
  end

  it "be empty after #clear" do
    add 'Pickaxe' => 'good book'

    Books.should.not.be.empty
    Books.clear
    Books.should.be.empty
  end

  it "should have right size" do
    Books.size.should == 0

    { 'Pickaxe' => 'good book',
      '1984' => 'scary',
      'Brave new World' => 'interesting',
    }.each_with_index do |(title, content), i|
      add title => content
      Books.size.should == i + 1
    end
  end

  it "should be Enumerable" do
    add 'Pickaxe' => 'good book', '1984' => 'scary'

    Books.each do |title, content|
      [title, content].compact.empty?.should == false
      Books[title].should == content
    end
  end

  it "should merge and merge!" do
    books = {'Pickaxe' => 'good book', '1984' => 'scary'}
    add books

    bnw = {'Brave new World' => 'interesting'}

    Books.merge(bnw).should == books.merge(bnw)
    Books[bnw.keys.first].should == nil

    Books.merge!(bnw).should == books.merge(bnw)
    Books[bnw.keys.first].should == bnw.values.first

    Books.size.should == 3
  end
end
