require 'ramaze'
require 'ramaze/spec/helper'

spec_require 'hpricot', 'sequel'

$LOAD_PATH.unshift base = __DIR__('..')
require 'start'

describe 'Wikore' do
  behaves_like 'http'
  ramaze :public_root => base/:public,
         :view_root   => base/:template

  def check_redirect(to = '/')
    response = yield
    response.status.should == 302
    response.body.should =~ /<a href="#{to}">/
  end

  def page_should_exist(name, *matches)
    page = get("/#{name}")
    page.status.should == 200
    matches.each do |match|
      page.body.should =~ match
    end
  end

  it 'should have no Main page' do
    page = get('/Main')
    page.status.should == 200
    page.body.should =~ /No Page known as 'Main'/
  end

  it 'should create a Main page' do
    check_redirect '/Main' do
      post('/page/create', 'title' => 'Main', 'text' => 'Newly created Main page')
    end

    matches = [
      /Newly created Main page/,
      /Version: 1/
    ]
    page_should_exist('Main', *matches)
  end

  it 'should update Main page' do
    check_redirect '/Main' do
      post('/page/save', 'title' => 'Main', 'text' => 'Newly updated Main page')
    end

    matches = [
      /Newly updated Main page/,
      /Version: 2/
    ]
    page_should_exist('Main', *matches)
  end

  it 'should maintain a backup' do
    matches = [
      /Newly created Main page/,
      /Version: 1/
    ]
    page_should_exist('Main/1', *matches)
  end

  it 'should revert' do
    get('/page/revert/Main')

    matches = [
      /Newly created Main page/,
      /Version: 1/
    ]
    page_should_exist('Main', *matches)
  end


  it 'should incrememt version of Main page' do
    (2..4).each do |n|
      post('/page/save', 'title' => 'Main', 'text' => 'updated Main page')

      matches = [ /updated Main page/, /Version: #{n}/ ]
      page_should_exist('Main', *matches)
    end
  end

  it 'should rename Main page to Other and back' do
    check_redirect '/Other' do
      get('/page/rename/Main/Other')
    end
    check_redirect '/Main' do
      get('/page/rename/Other/Main')
    end
  end

  it 'should delete Main page' do
    get('/page/delete/Main')

    page_should_exist('Main', /No Page known as 'Main'/)
  end

  it 'should fail if create/save is not POSTed to' do
    check_redirect '/' do
      get('/page/save', 'title' => 'Main', 'text' => 'Newly updated Main page')
    end
    check_redirect '/' do
      get('/page/create', 'title' => 'Main', 'text' => 'Newly updated Main page')
    end
  end

  FileUtils.rm_f(DB_FILE)
end
