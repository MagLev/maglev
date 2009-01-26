#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class BaseController < Ramaze::Controller
  template :foo, :bar
  template :one, self, :another

  def test() 'test' end
end

class MainController < BaseController
  engine :None
end

describe 'Controller' do
  behaves_like 'http'
  ramaze

  it 'should allow sub-classing MainController' do
    get('/test').body.should == 'test'
  end

  it 'should respect template mappings set in superclass, with no explicit controller' do
    # The template file it should use is view/bar.xhtml, as the template mapping doesn't
    # specify a controller, so it will be implicitly relative to MainController.
    get('/foo').body.should == 'bar'
  end

  it 'should respect template mappings set in superclass, with an explicit controller' do
    # Note that the template file it should use is view/base/another.xhtml, because
    # BaseController explicitly specifies the template mapping in relation to self.
    get('/one').body.should == 'another'
  end
end
