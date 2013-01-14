require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter#increment_by" do

  it "adds x to the counter" do
    counter = RCCounter.new
    counter.increment_by 10
    counter.value.should == 10
  end

end
