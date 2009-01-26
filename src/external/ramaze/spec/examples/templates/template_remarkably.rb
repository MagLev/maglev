require 'spec/helper'

spec_require 'remarkably/engines/html'
require 'examples/templates/template_remarkably'

describe 'Template Remarkably' do
  behaves_like 'http'
  ramaze

  it '/' do
    get('/').body.strip.should ==
      "<a href=\"/\">Home</a> | <a href=\"/internal\">internal</a> | <a href=\"/external\">external</a>"
  end

  %w[/internal /external].each do |url|
    it url do
      html = get(url).body
      html.should.not == nil
      html.should =~ %r{<title>Template::Remarkably (internal|external)</title>}
      html.should =~ %r{<h1>The (internal|external) Template for Remarkably</h1>}
    end
  end
end
