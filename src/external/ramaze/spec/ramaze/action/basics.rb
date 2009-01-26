require 'spec/helper'

describe 'Action() basics' do
  it 'should have useful defaults' do
    action = Ramaze::Action()

    action.params.should == []
    action.method.should == nil
    action.template.should == nil
  end

  it 'should sanitize parameters' do
    action = Ramaze::Action :params => [[1],[2],nil,'%20'],
                            :method => :foo

    action.params.should == ['1', '2', ' ']
    action.method.should == 'foo'
  end

  it 'should be transformable in an hash' do
    # we need to supply a :controller because internal accessor methods
    # will use it to build some defaults.
    # TODO: in Action.new() raise on init :controller or use a default
    hsh = Ramaze::Action(:controller=>Object).to_hash
    hsh[:method].should == nil
    hsh[:binding].should.instance_of?(Binding)
    hsh[:controller].should == Object
    hsh[:engine].should == Ramaze::Template::Ezamar
    hsh[:params].should == []
    hsh[:path].should == nil
    hsh[:instance].should.instance_of?(Object)
    hsh[:template].should == nil
  end


end
