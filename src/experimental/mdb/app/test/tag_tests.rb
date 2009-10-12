# MiniTest suite for the application's Model
require 'rubygems'
require 'minitest/spec'
require 'blog.rb'

MiniTest::Unit.autorun

SECONDS_PER_DAY = 60 * 60 * 24

describe Tag do
  before do
    @tags = Hash.new
    @tagged = [Object.new, Object.new, Object.new ]
    %w(foo bar quux).each_with_index do |tn,idx|
      t = Tag.new(tn)
      @tags[idx] = t
      @tagged.each { |o| t << o }
    end
  end

  it 'Implements each and << properly' do
    objs = []
    a_tag = @tags.values[0]
    a_tag.each { |el| objs << el }
    objs.must_equal @tagged
  end

  it 'finds tags by name' do
    x = Tag.find_by_name @tags, :quux
    x.size.must_equal 1
  end
end
