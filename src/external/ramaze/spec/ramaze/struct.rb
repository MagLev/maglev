require 'spec/helper'

Point = Ramaze::Struct.new(:x,:y)

describe "Ramaze::Struct" do
  describe '#values_at' do
    @point = Point.new(1,2)

    it "should access a single value" do
      @point.values_at(:x).should == [1]
    end

    it "should access multiple values" do
      @point.values_at(:x,:y).should == [1,2]
    end

    it "should access values regardless of order" do
      @point.values_at(:y,:x).should == [2,1]
    end

    it "should get same value twice" do
      @point.values_at(:x,:x).should == [1,1]
    end

    it "should raise on wrong value" do
      should.raise(NameError){
        @point.values_at(:k)
      }
    end

    it "should work with strings" do
      @point.values_at('x').should == [1]
    end

    it "should work with numbers (ruby compat)" do
      @point.values_at(0).should == [1]
    end

    should 'work with ranges' do
      @point.values_at(0..1).should == [1,2]
    end

    should 'work with multiple ranges' do
      @point.values_at(0..1, 0..1).should == [1,2,1,2]
    end
  end
end
