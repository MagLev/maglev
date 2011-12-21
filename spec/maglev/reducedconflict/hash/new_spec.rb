require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rchash'

describe "RCHash.new" do

  before do
    @hash = RCHash.new
  end

  it "returns an instance of RCHash" do
    @hash.should be_an_instance_of(RCHash)
  end

  it "creates an empty hash if passed no arguments" do
    @hash.size.should == 0
    @hash.empty?.should be_true
  end

end
