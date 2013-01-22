require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter#decrement" do

  it "subtracts 1 from the counter" do
    counter = RCCounter.new
    counter.decrement
    counter.value.should == -1
  end

end
