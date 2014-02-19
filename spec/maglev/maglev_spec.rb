require File.expand_path '../../spec_helper', __FILE__

describe 'Maglev' do

  describe '#root' do
    it 'references Maglev::PERSISTENT_ROOT' do
      Maglev.root.should == Maglev::PERSISTENT_ROOT
    end
  end

  describe '#[]' do
    it 'updates Maglev::PERSISTENT_ROOT' do
      message = 'hello world'
      Maglev::PERSISTENT_ROOT[:message] = message
      Maglev[:message].should == message
    end
  end

  describe '#[]=' do
    it 'updates Maglev::PERSISTENT_ROOT' do
      Maglev[:message] = 'hello world'
      Maglev[:message].should == Maglev::PERSISTENT_ROOT[:message]
    end
  end

end
