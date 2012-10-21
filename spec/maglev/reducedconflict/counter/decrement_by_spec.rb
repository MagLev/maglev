require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter#decrement_by" do

  describe "with only amount" do
    it "subtracts x from the counter" do
      counter = RCCounter.new
      counter.decrement_by 10
      counter.value.should == -10
    end
  end

  describe "with amount, guard and block" do
    before do
      @counter = RCCounter.new
    end

    describe "when guard is hit" do
      it "executes the block" do
        @counter.decrement_by(10, -1) do
          'block executed'
        end.should == 'block executed'
      end
    end

    describe "when guard is missed" do
      it "decrements the value" do
        @counter.decrement_by(5, -7) do
          "won't get executed"
        end
        @counter.value.should == -5
      end

      it "returns self" do
        @counter.decrement_by(5, -7) do
          "won't get executed"
        end.should equal(@counter)
      end
    end
  end

end
