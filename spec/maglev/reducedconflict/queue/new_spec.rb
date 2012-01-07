require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rcqueue'

describe "RCQueue.new" do

  it "returns an instance of RCQueue" do
    RCQueue.new.should be_an_instance_of(RCQueue)
  end

end
