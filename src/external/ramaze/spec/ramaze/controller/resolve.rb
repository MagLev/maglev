#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class MainController < Ramaze::Controller
  engine :None

  define_method('file.ext') { 'file.ext' }
  define_method('css/file.css') { 'file.css' }
  define_method(:path/:to/:js/'file.js') { 'file.js' }

  define_method(:other/:greet/:other) { @greet = 'hi' }
end

describe 'Controller resolving' do
  behaves_like 'http'
  ramaze :view_root => __DIR__(:view)

  it 'should work with .' do
    get('/file.ext').body.should == 'file.ext'
  end

  it 'should work with /' do
    get('/css/file.css').body.should == 'file.css'
    get('/path/to/js/file.js').body.should == 'file.js'
  end

  it 'should find templates' do
    get('/other/greet/other').body.should == '<html>Other: hi</html>'
  end
end
