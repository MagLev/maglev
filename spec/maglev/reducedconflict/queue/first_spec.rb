require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rcqueue'

describe "RCQueue#first" do

  it "returns the first element for a non-empty queue" do
    q = RCQueue.new
    q.add 1
    q.add 2
    q.add 3
    q.first.should == 1
    q.remove
    q.first.should == 2
  end

  it "returns nil for an empty queue" do
    RCQueue.new.first.should be_nil
  end

end
