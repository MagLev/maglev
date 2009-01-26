require 'ramaze'
require 'ramaze/spec/helper'

spec_require 'hpricot', 'bluecloth'

$LOAD_PATH.unshift base = __DIR__('..')
require 'start'

describe 'wiktacular' do
  behaves_like 'http'
  NEWPAGE = "newpagename"

  def check_page(name)
    page = get('/'+name)
    page.status.should == 200
    page.body.should.not == nil

    doc = Hpricot(page.body)
    doc.at('title').inner_html.should == 'Wiktacular'

    menu = doc.search('div#menu>a')
    menu[0].inner_html.should == 'Home'
    menu[1].inner_html.should == 'New Entry'

    navigation = doc.search('div#navigation>div>a')
    %w[link main markdown testing].each do |link|
      navigation.map{|n| n.inner_html }.sort.should.include(link)
    end

    manipulate = doc.search('div#manipulate>a')
    manipulate.map{|m| m.inner_html }.should ==
      %w[Edit Delete Revert Unrevert]

    doc
  end

  it 'should start' do
    ramaze :public_root => base/:public,
           :view_root   => base/:template
    get('/').status.should == 200
  end

  it 'should have main page' do
    check_page('/main')
  end

  it 'should have link page' do
    check_page('/link')
  end

  it 'should have markdown page' do
    check_page('/markdown')
  end

  it 'should have testing page' do
    check_page('/testing')
  end


  it 'should not have foobar page' do
    doc = check_page('/foobar')
    doc.at('div#text').inner_html.strip.should == 'No Entry'
  end

  it 'should allow page editing' do
    doc = check_page('/edit/main')
    form = doc.at('div#content>form')
    form.at('input[@type=text]')['value'].should == 'main'
    form.at('textarea').inner_html.should.match(/# Hello, World/)
    form.at('a').inner_html.should == 'cancel'
    form.at('a')['href'].should == '/main'
  end

  def edit_page(name, text='new text')
    page = post('/save','handle'=>name,'text'=>text)
    page.status.should == 302
    page.location.should == '/index/'+name
  end

  def delete_page(name)
    page = get('/delete/'+name)
    page.status.should == 302
    page.location.should == '/'
  end

  def revert_page(name)
    page = get('/revert/'+name)
    page.status.should == 302
    page.location.should == '/'+name
  end

  def unrevert_page(name)
    page = get('/revert/'+name)
    page.status.should == 302
    page.location.should == '/'+name
  end

  it 'editing should create page' do
    edit_page(NEWPAGE, 'new text')
    doc = check_page(NEWPAGE)
    doc.at('div#text').inner_html.strip.should == '<p>new text</p>'
    delete_page(NEWPAGE)
  end

  it 'editing should modify page' do
    edit_page(NEWPAGE, 'text text')
    doc = check_page(NEWPAGE)
    doc.at('div#text').inner_html.strip.should == '<p>text text</p>'
    edit_page(NEWPAGE,'some other text')
    doc = check_page(NEWPAGE)
    doc.at('div#text').inner_html.strip.should == '<p>some other text</p>'
    delete_page(NEWPAGE)
  end

  it "should be possible to revert changes" do
    edit_page(NEWPAGE, 'first text')
    edit_page(NEWPAGE, 'second text')
    doc = check_page(NEWPAGE)
    doc.at('div#text').inner_html.strip.should == '<p>second text</p>'
    revert_page(NEWPAGE)
    doc = check_page(NEWPAGE)
    doc.at('div#text').inner_html.strip.should == '<p>first text</p>'
  end

  it "should be possible to unrevert changes" do
    edit_page(NEWPAGE, 'first text')
    edit_page(NEWPAGE, 'second text')
    revert_page(NEWPAGE)
    doc = check_page(NEWPAGE)
    doc.at('div#text').inner_html.strip.should == '<p>first text</p>'
    unrevert_page(NEWPAGE)
    doc = check_page(NEWPAGE)
    doc.at('div#text').inner_html.strip.should == '<p>first text</p>'
  end

  it "should convert [[#{NEWPAGE}]] to existing link" do
    edit_page(NEWPAGE, "Link: [[#{NEWPAGE}]]")
    doc = check_page(NEWPAGE)
    a = doc.at('div#text>p>a')
    a['class'].should == 'exists'
    a['href'].should == NEWPAGE
    a.inner_html.should == NEWPAGE
  end

  it "should convert [[chunky bacon]] to nonexisting link" do
    edit_page(NEWPAGE, "Link: [[chunky bacon]]")
    doc = check_page(NEWPAGE)
    a = doc.at('div#text>p>a')
    a['class'].should == 'nonexists'
    a['href'].should == "chunky+bacon"
    a.inner_html.should == "chunky bacon"
  end

  should 'delete page for cleanup' do
    WikiEntry.new(NEWPAGE).delete.first.should =~ /#{NEWPAGE}$/
  end
end
