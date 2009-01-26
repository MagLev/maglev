#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCElementController < Ramaze::Controller
  engine :Ezamar

  def index
    "The index"
  end

  def elementy
    "<Page>#{index}</Page>"
  end

  def nested
    "<Page> some stuff <Page>#{index}</Page> more stuff </Page>"
  end

  def with_params(*params)
    hash = Hash[*params.flatten].map{|k,v| %{#{k}="#{v}"}}.join(' ')
    %{<PageWithParams #{hash}></PageWithParams>}
  end

  def little
    %{<PageLittle />}
  end

  def little_params(*params)
    hash = Hash[*params.flatten].map{|k,v| %{#{k}="#{v}"}}.join(' ')
    %{<PageLittleWithParams #{hash} />}
  end

  def templating(times)
    %{<PageWithTemplating times="#{times}" />}
  end
end

class Page < Ezamar::Element
  def render
    %{ <wrap> #{content} </wrap> }
  end
end


class PageWithParams < Ezamar::Element
  def render
    ivs = (instance_variables - ['@content', :@content])
    ivs.inject({}){|s,v| s.merge(v.to_s => instance_variable_get(v)) }.inspect
  end
end

class PageLittle < Ezamar::Element
  def render
    "little"
  end
end

class PageLittleWithParams < Ezamar::Element
  def render
    ivs = (instance_variables - ['@content', :@content])
    ivs.inject({}){|s,v| s.merge(v.to_s => instance_variable_get(v)) }.inspect
  end
end

class PageWithTemplating < Ezamar::Element
  def render
    (1..@times).to_a.join(', ')
  end
end

describe "Element" do
  ramaze(:mapping => {'/' => TCElementController})

  def retrieve(*url)
    Ramaze::Controller.handle(*url).strip
  end

  it "simple request" do
    retrieve('/').should == "The index"
  end

  it "with element" do
    retrieve('/elementy').should == "<wrap> The index </wrap>"
  end

  it "nested element" do
    retrieve('/nested').should == "<wrap>  some stuff  <wrap> The index </wrap>  more stuff  </wrap>"
  end

  it "with_params" do
    retrieve('/with_params/one/two').should == {'@one' => 'two'}.inspect
  end

  it "little" do
    retrieve('/little').should == 'little'
  end

  it "little params" do
    retrieve('/little_params/one/eins').should == {'@one' => 'eins'}.inspect
  end

  it "templating" do
    retrieve('/templating/10').should == (1..10).to_a.join(', ')
  end
end
