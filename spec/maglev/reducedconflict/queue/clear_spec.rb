require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rcqueue'

describe "RCQueue#clear" do

  before do
    @q = RCQueue.new
    @q.add 1
    @q.add 2
    @q.add 3
  end

  it "removes all elements" do
    @q.clear
    @q.empty?.should be_true
    @q.size.should == 0
  end

  it "returns an ordered array of elements" do
    @q.clear.should == [1, 2, 3]
  end

end
