# This causes a local jump error.
require 'rubygems'
require 'erb'
require 'tilt'

TDIR = File.join(File.dirname(__FILE__), 'test_data')
X      = File.join(TDIR, 'x.erb')
LAYOUT = File.join(TDIR, 'layout.erb')
class C
  # This include turns on the class << self in Tilt
  include Tilt::CompileSite

  def render(x, &block)
    template = Tilt.new(X)
    layout   = Tilt.new(LAYOUT)

    # render template
    output = template.render(self, { }, &block)

    # render layout
    output = layout.render { output }
    p output
  end
end

C.new.render(10) { "block to render" }
true
