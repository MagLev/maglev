require 'rubygems'
require 'minitest/spec'

MiniTest::Unit.autorun

describe SimplePost do
  before do
    Maglev::PERSISTENT_ROOT[SimplePost].clear
    5.times do |i|
      SimplePost.persistent_new(:title => "Title #{i}", :text => "Text #{i}")
    end
  end

  it "responds to each correctly" do
    titles = SimplePost.map { |p| p.title }
    titles.size.must_equal 5
  end
end
