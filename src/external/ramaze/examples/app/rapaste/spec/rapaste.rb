require 'ramaze'
require 'ramaze/spec/helper'

spec_require 'hpricot', 'uv', 'sequel'

$LOAD_PATH.unshift base = __DIR__('..')
require 'start'

describe 'RaPaste' do
  behaves_like 'http'
  ramaze :public_root => base/:public,
         :view_root   => base/:view

  it 'should show an empty list on the list page' do
    page = get('/list')
    Hpricot(page.body).at('p.list_empty').inner_html.strip.
      should == 'No pastes available yet, go on and <a href="/">Add one</a>'
  end

  it 'should have a link to the new paste form' do
    page = get('/list')
    Hpricot(page.body).at('#menu a[@href=/]').inner_text.should == 'New'
  end

  it 'should show a new paste form' do
    page = get('/')
    form = Hpricot(page.body).at('form#new_paste')
    form[:action].should == '/save'
    form[:method].should == 'POST'
    form.at(:textarea)[:name].should == 'text'
    form.at('select/option[@value=plain_text]').inner_text.should == 'Plain Text'
  end

  it 'should create a new paste' do
    page = post('/save', 'syntax' => 'plain_text', 'text' => 'spec paste')
    page.status.should == 302
    page.original_headers['Location'].should == '/1'
  end

  it 'should show the new paste in plain text' do
    page = get('/1.txt')
    page.body.should == 'spec paste'
  end

  it 'should show the new paste in html' do
    page = get('/1')
    (Hpricot(page.body)/'div#paste_body').inner_text.should =~ /spec paste/
  end

  FileUtils.rm_f(DB_FILE)
end
