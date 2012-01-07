require File.expand_path('../../../../../spec_helper', __FILE__)
require 'maglev/rcqueue'

describe :rcqueue_add, :shared => true do
  it "returns the receiver" do
    q = RCQueue.new
    q.send(@method, :a).should == :a
  end

  it "adds an element to the queue" do
    q = RCQueue.new
    100.times do
      q.send(@method, :a)
    end
    q.size.should == 100
  end
end
