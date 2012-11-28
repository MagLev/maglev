require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter#increment" do

  it "adds 1 to the counter" do
    counter = RCCounter.new
    counter.increment
    counter.value.should == 1
  end

end
