require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter.new" do

  it "returns an instance of RCCounter" do
    RCCounter.new.should be_an_instance_of(RCCounter)
  end

end
