require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rcqueue'

describe "RCQueue#empty?" do

  before do
    @q = RCQueue.new
  end

  it "returns true if size is 0" do
    @q.empty?.should be_true
  end

  it "returns false if size is not 0" do
    @q << 1
    @q.empty?.should be_false
  end

end
