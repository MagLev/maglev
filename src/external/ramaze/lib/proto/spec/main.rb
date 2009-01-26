require 'ramaze'
require 'ramaze/spec/helper'

require __DIR__('../start')

describe MainController do
  behaves_like 'http', 'xpath'
  ramaze :view_root => __DIR__('../view'),
         :public_root => __DIR__('../public')

  it 'should show start page' do
    got = get('/')
    got.status.should == 200
    puts got.body
    got.at('//title').text.strip.should ==
      MainController.new.index
  end

  it 'should show /notemplate' do
    got = get('/notemplate')
    got.status.should == 200
    got.at('//div').text.strip.should ==
      MainController.new.notemplate
  end
end
