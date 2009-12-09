# MiniTest suite for the app model
# This test run by MRI
require 'rubygems'
require 'minitest/spec'
require 'app_model'

MiniTest::Unit.autorun

SECONDS_PER_DAY = 60 * 60 * 24

describe AppModel do
  before do
    @data = Hash.new
    now = Time.now
    10.times do |i|
      created_on = now -  SECONDS_PER_DAY * i # create a time-stamp i days ago
      @data = Post.new()
    end
  end

end
