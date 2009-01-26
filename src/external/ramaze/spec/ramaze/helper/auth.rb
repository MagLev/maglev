#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCAuthHelperController < Ramaze::Controller
  map '/'
  helper :auth

  def index
    self.class.name
  end

  def session_inspect
    session.inspect
  end

  def secured
    "Secret content"
  end
  before(:secured){ login_required }
end

class TCAuthHashHelperController < TCAuthHelperController
  map '/hash'
  trait :auth_table => {
      'manveru' => '5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8'
    }
end

class TCAuthMethodHelperController < TCAuthHelperController
  map '/method'
  trait :auth_table => :auth_table

  def auth_table
    { 'manveru' => '5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8' }
  end
end

class TCAuthLambdaHelperController < TCAuthHelperController
  map '/lambda'
  trait :auth_table => lambda{
      { 'manveru' => '5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8' }
    }
end

describe "StackHelper" do
  behaves_like 'browser'

  ramaze :adapter => :webrick
  [ TCAuthHashHelperController,
    TCAuthMethodHelperController,
    TCAuthLambdaHelperController
  ].each do |controller|

    it controller.to_s do
      Browser.new(Ramaze::Global.mapping.invert[controller]) do
        get('/secured').should == ''
        post('/login', 'username' => 'manveru', 'password' => 'password')
        get('/secured').should == 'Secret content'
        get('/logout')
        get('/secured').should == ''
      end
    end
  end
end
