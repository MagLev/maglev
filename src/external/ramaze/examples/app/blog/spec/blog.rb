require 'ramaze'
require 'ramaze/spec/helper'

spec_require 'hpricot', 'sequel'

$LOAD_PATH.unshift base = __DIR__('..')
require 'start'

describe 'Blog' do
  behaves_like 'http'
  ramaze :public_root => base/:public,
         :view_root   => base/:view

  after do
    Entry.each{|e| e.delete unless e.id == 1 }
  end

  def check_page(name = '')
    page = get("/#{name}")
    page.status.should == 200
    page.body.should.not == nil

    doc = Hpricot(page.body)
    doc.at('title').inner_html.should == 'Blog'
    doc.at('h1').inner_html.should == 'Blog'

    doc.search('div#entries').size.should == 1

    doc
  end

  def create_page(title,content)
    page = post('/create','title'=>title,'content'=>content)
    page.status.should == 302
    page.location.should == '/'
  end

  it 'should have main page' do
    doc = check_page
    doc.at('div#actions>a').inner_html.should == 'new entry'
    doc.search('div.entry').size.should == 1
  end

  it 'should have new entry page' do
    doc = check_page('new')
    form = doc.at('div.entry>form')
    form.at('input[@name=title]')['value'].should == ''
    form.at('textarea').inner_html.should == ''
    form.at('input[@type=submit]')['value'].should == 'Add Entry'
  end

  it 'should add new pages' do
    create_page('new page', 'cool! a new page')
    doc = check_page
    entry = doc.search('div.entry')
    entry.size.should == 2
    entry = entry.last

    entry.at('div.title').inner_html == 'new page'
    entry.at('div.content').inner_html == 'cool! a new page'
  end

  it 'should edit existing pages' do
    create_page('new page', 'cool! a new page')
    post('/save','id'=>'2','title'=>'new title','content'=>'bla bla')
    doc = check_page
    entries = doc/'div.entry'
    entries.size.should == 2
    entry = entries.first

    entry.at('div.title').inner_html == 'new title'
    entry.at('div.content').inner_html == 'bla bla'
  end

  it 'should delete existing pages' do
    create_page("page to delete", 'content')
    entries = check_page/'div.entry'
    entries.size.should == 2
    delete_link = entries.last.at("a:contains('delete')")
    page = get(delete_link[:href])
    page.status.should == 302
    page.location.should == '/'
    (check_page/'div.entry').size.should == 1
  end

  FileUtils.rm_f(__DIR__('../blog.db'))
end
