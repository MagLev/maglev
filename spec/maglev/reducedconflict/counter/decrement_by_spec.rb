require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter#decrement_by" do

  it "subtracts x from the counter" do
    counter = RCCounter.new
    counter.decrement_by 10
    counter.value.should == -10
  end

end
