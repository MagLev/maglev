require 'ramaze'
require 'ramaze/spec/helper'

spec_require 'hpricot'

$LOAD_PATH.unshift base = __DIR__('..')
require 'start'

describe 'todolist' do
  behaves_like 'http'

  def h_get(*args)
    Hpricot(get(*args).body)
  end

  def task_titles
    doc = h_get('/')
    doc.search("td[@class='title']").
      map{|t| t.inner_html.strip}.sort
  end

  def tasks
    (h_get('/')/:tr)
  end

  def task(name)
    tasks.find do |task|
      (task/:td).find do |td|
        td['class'] == 'title' and
        td.inner_html.strip == name
      end
    end
  end

  def spectask
    task('spectask')
  end

  def spectask_status
    spectask.search("td[@class='status']").inner_html.strip
  end

  def error_on_page(url, response)
    doc = h_get(url, :cookie => response.headers['Set-Cookie'])

    error = doc.search("div[@class='error']")
    error.inner_html.strip
  end

  it 'should start' do
    ramaze :public_root => base/:public,
           :view_root   => base/:template
    get('/').status.should == 200
  end

  it 'should have no empty mainpage' do
    get('/').body.should.not == nil
  end

  it 'should have two preset tasks' do
    task_titles.should == %w[Laundry Wash\ dishes]
  end

  it 'should have a link to new tasks' do
    doc = h_get('/')
    link = (doc/:a).find{|a| a.inner_html == 'New Task'}
    link['href'].should == '/new'
  end

  it 'should have a page to create new tasks' do
    get('/new').body.should.not == nil
  end

  it 'should have a form to create a tasks on the /new page' do
    doc = h_get('/new')
    form = doc.at :form
    form.should.not == nil
    input = form.at(:input)
    input['type'].should == 'text'
    input['name'].should == 'title'
  end

  it 'should POST new task and redirect to /' do
    result = post('/create', 'title' => 'spectask')
    result.status.should == 302
  end

  it 'should show have the new task' do
    task_titles.should.include('spectask')
  end

  it 'should toggle the spectask' do
    get('/close/spectask').status.should == 302
    spectask.should.not == nil
    spectask_status.should == 'done'
    get('/open/spectask').status.should == 302
    spectask.should.not == nil
    spectask_status.should == 'not done'
  end

  it 'should raise on modifying a not existing task' do
    %w[open close].each do |action|
      response = get("/#{action}/nothere", :referrer => "/")
      response.status.should == 302
      response.original_headers['Location'].should == '/'
      error_on_page('/', response).should == "No such Task: `nothere'"
    end
  end

  it 'should delete the new task' do
    get('/delete/spectask').status.should == 302
    task_titles.should.not.include('spectask')
  end

  it 'should not create empty tasks but show a subtle error message' do
    response = post('/create', 'title' => '', :referrer => "/new")

    response.status.should == 302
    response.original_headers['Location'].should == '/new'

    error_on_page('/new', response).should == 'Please enter a title'
  end

  it 'should escape harmful titles' do
    response = post('/create', 'title' => '#{puts "gotcha"}')

    task('#{puts "gotcha"}').should.be.nil
    task('&#35;{puts &quot;gotcha&quot;}').should.not.be.nil
  end

  FileUtils.rm('todolist.db') rescue nil
end
