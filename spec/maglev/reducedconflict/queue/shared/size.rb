require File.expand_path('../../../../../spec_helper', __FILE__)
require 'maglev/rcqueue'

describe :rcqueue_size, :shared => true do
  it "returns 0 when queue is empty" do
    RCQueue.new.send(@method).should == 0
  end

  it "returns number of elements" do
    q = RCQueue.new
    100.times do
      q << 1
    end
    q.send(@method).should == 100
  end
end
