require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rchash'

describe "RCHash#values" do
  it "returns an empty array if hash is empty" do
    hash = RCHash.new
    hash.values.should == []
  end

  it "returns an array of hash values" do
    hash = RCHash.new
    hash[:a] = 1
    hash[:b] = 2
    hash[:c] = 3
    hash.values.should == [1, 2, 3]
  end

end
