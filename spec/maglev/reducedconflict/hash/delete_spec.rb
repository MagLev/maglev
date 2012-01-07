require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rchash'

describe "RCHash#delete" do
  it "returns the value if the key exists" do
    hash = RCHash.new
    hash[:a] = 1
    hash.delete(:a).should == 1
  end

  it "returns nil if the key does not exist" do
    hash = RCHash.new
    hash[:a] = 1
    hash.delete(:b).should be_nil
  end
end
