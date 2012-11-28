require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rccounter'

describe "RCCounter#value" do
  describe "new instance" do
    it "returns 0" do
      RCCounter.new.value.should == 0
    end
  end
end
