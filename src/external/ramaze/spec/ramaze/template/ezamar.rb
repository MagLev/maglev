#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCTemplateController < Ramaze::Controller
  map :/
  view_root __DIR__(:ezamar)
  engine :Ezamar

  def index text
    @text = text
  end

  def sum num1, num2
    @num1, @num2 = num1.to_i, num2.to_i
  end

  def nested key, value
    instance_variable_set("@#{key}", value)
  end

  def internal *args
    @args = args
    '<?r i = 2 ?>#{i * i} #{@args.inspect} on the table'
  end

  def combined
    @a = 'boo'
  end
end


describe "Ezamar" do
  behaves_like 'http'
  ramaze

  it "hello world" do
    get('/World').body.should == 'Hello, World!'
    get('/You').body.should == 'Hello, You!'
  end

  it "summing" do
    get('/sum/1/2').body.should == '3'
  end

  it "nasty nested stuff" do
    get('/nested/foo/bar').body.should == 'bar'
  end

  it "template inside controller" do
    get('/internal').body.should == '4 [] on the table'
    get('/internal/foo').body.should == '4 ["foo"] on the table'
  end

  it "without method" do
    get('/file_only').body.should == "This is only the file"
  end

  it "combined" do
    get('/combined').body.should == 'boo'
  end
end
