require 'rubygems'
require 'minitest/spec'

MiniTest::Unit.autorun

describe SimplePost do
  it "Adds newly created posts to the persistent root" do
    p0 = SimplePost.persistent_new(:title => "The Title", :text => "Some text")
    p1 = SimplePost.get(p0.__id__)
    p1.must_equal p0
  end

  it "Returns the newly created posts" do
    posts = SimplePost.all
    titles = posts.map{ |p| p.title }
    titles.must_include "The Title"  # must_include is backwards....
  end
end

describe SimpleTag do
  it "Adds newly created tags to the persistent root" do
    t0 = SimpleTag.persistent_new("magleviathon")
    t1 = SimpleTag.get(t0.__id__)
    t1.must_equal t0
  end
end
