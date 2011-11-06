require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rchash'

describe "RCHash#keys" do
  it "returns an empty array if hash is empty" do
    hash = RCHash.new
    hash.keys.should == []
  end

  it "returns an array of hash keys" do
    hash = RCHash.new
    hash[:a] = 1
    hash[:b] = 2
    hash[:c] = 3
    keys = hash.keys
    keys.length.should == 3
    keys.should include(:a)
    keys.should include(:b)
    keys.should include(:c)
  end

end
