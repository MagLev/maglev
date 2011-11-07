require File.expand_path('../../../../../spec_helper', __FILE__)
require 'maglev/rcqueue'

describe :rcqueue_remove, :shared => true do

  it "removes the leading element" do
    q = RCQueue.new
    q.add 1
    q.add 2
    q.add 3
    q.send(@method)
    q.to_a.should == [2,3]
  end

  it "returns the leading element" do
    q = RCQueue.new
    q.add 1
    q.add 2
    q.add 3
    q.send(@method).should == 1
  end

  it "returns nil if queue is empty" do
    q = RCQueue.new
    q.send(@method).should be_nil
  end
end
