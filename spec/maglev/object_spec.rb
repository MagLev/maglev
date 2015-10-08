require File.expand_path '../../spec_helper', __FILE__

describe 'Object' do

  describe '#become' do
    it 'swaps identities of the objects' do
      foo, bar = 'foo', 'bar'
      foo.become bar
      foo.should == 'bar'
    end
  end

end
