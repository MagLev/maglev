require 'rubygems'
require 'ramaze'

# delete cached css after inline template is changed
module Ramaze::SourceReloadHooks
  module_function
  def after_safe_load file
    Ramaze::Cache.actions.delete '/css/style.css' if file == __FILE__
  end
end

class CSSController < Ramaze::Controller
  engine :Sass

  helper :aspect
  before_all do
    response['Content-Type'] = 'text/css'
    nil
  end

  define_method('style.css') do
    %(
body
  font:
    family: sans-serif
    size: 11px
  margin: 0.5em
  padding: 1em
    )
  end

  helper :cache
  cache 'style.css'
end

# http://localhost:81/css/style.css
Ramaze.start :adapter => :mongrel, :port => 81
