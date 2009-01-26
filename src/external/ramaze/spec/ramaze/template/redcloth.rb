require 'spec/helper'

spec_require 'erubis'
spec_require 'redcloth'

class TCTemplateRedClothController < Ramaze::Controller
  view_root __DIR__(:redcloth)
  engine :RedCloth

  def index
    'RedCloth Index'
  end

  def external
    @num = 1
  end

  def internal
    "h2. <%= 1 + 1 %>"
  end
end

describe "RedCloth templates" do
  behaves_like 'http'
  ramaze(:mapping => {'/' => TCTemplateRedClothController})

  it "index" do
    get('/').body.should == '<p>RedCloth Index</p>'
  end

  it "external" do
    get('/external').body.should == '<h1>1</h1>'
  end

  it "internal" do
    get('/internal').body.should == "<h2>2</h2>"
  end
end
