require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rchash'

describe "RCHash#empty?" do
  it "returns true if hash has no elements" do
    hash = RCHash.new
    hash.empty?.should be_true
  end

  it "returns false if hash has elements" do
    hash = RCHash.new
    hash[:a] = 1
    hash.empty?.should be_false
  end
end
