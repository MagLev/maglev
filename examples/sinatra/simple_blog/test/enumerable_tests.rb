require 'rubygems'
require 'minitest/spec'

MiniTest::Unit.autorun

Exception.install_debug_block do |e|
  puts "=== (#{e.class}): ====== #{e}"
  case e
  when ArgumentError, NoMethodError
    nil.pause
  end
end

describe Post do
  before do
    5.times do |i|
      Post.new(:title => "Title #{i}", :text => "Text #{i}")
    end
  end

  it "responds to each correctly" do
    titles = Post.map { |p| p.title }
    titles.size.must_equal 5
  end
end
