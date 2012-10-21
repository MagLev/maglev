require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter#decrement_if_negative" do

  describe "when value is positive" do
    it "subtracts 1 from the counter" do
      counter = RCCounter.new
      counter.increment_by 10
      counter.decrement_if_negative { "won't get executed" }
      counter.value.should == 9
    end
  end

  describe "when value is negative" do
    it "executes the block" do
      counter = RCCounter.new
      result = counter.decrement_if_negative { 'block executed' }
      result.should == 'block executed'
    end
  end

end
