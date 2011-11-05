require File.expand_path('../../../../../spec_helper', __FILE__)
require 'maglev/rchash'

describe :rchash_length, :shared => true do
  it "returns the number of elements" do
    hash = RCHash.new
    hash.send(@method).should == 0
    hash[:a] = 1
    hash.send(@method).should == 1
  end
end
