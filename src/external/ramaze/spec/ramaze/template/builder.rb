require 'spec/helper'

spec_require 'builder', 'hpricot'

FRAMEWORKS = {
  'ramaze' => 'ruby',
  'symfony' => 'php',
  'django' => 'python' }

class TCTemplateBuilder < Ramaze::Controller
  view_root 'spec/ramaze/template/builder'
  map '/'
  engine :Builder

  def external
    @frameworks = FRAMEWORKS
  end

  def internal
    external
    %q[
      @frameworks.each do |name, lang|
        xml.framework {|f| f.name(name); f.language(lang) }
      end
    ]
  end
end

describe "Builder" do
  behaves_like 'http'
  ramaze

  def check(string)
    doc = Hpricot(string)
    got = (doc/:framework).map{|f|
      [ f.at(:name).inner_text,
        f.at(:language).inner_text ] }
    Hash[*got.flatten].should == FRAMEWORKS
  end

  it "should render xml from files" do
    r = get('/external')
    r.status.should == 200
    r.headers['Content-Type'].should == 'application/xml'
    check(r.body)
  end

  it 'should render internal templates' do
    check get('/internal').body
  end
end
