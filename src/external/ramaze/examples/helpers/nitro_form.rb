require 'rubygems'
require 'ramaze'

gem 'facets', '=1.4.5'
require '/home/manveru/prog/projects/nitroproject/glycerin'
require 'nitro'
require 'og'

class Article
  attr_accessor :title, String
end

Og.setup :store => :sqlite, :destroy => true

class MainController < Ramaze::Controller
  helper :nitroform

  def index
    form(Article.new).to_s
  end
end

Ramaze.start
