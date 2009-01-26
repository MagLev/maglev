require 'rubygems'
require 'ramaze'

# require YAML based localization
require 'ramaze/tool/localize'

# Activate localization
# Setup localization options
class Ramaze::Tool::Localize
  Ramaze::Dispatcher::Action::FILTER << self

  trait :default_language => 'en',
        :languages => %w[ en ja cn es de it ],
        :file => lambda{|l| Ramaze::Global.root/"locale/#{l}.yaml" }
  # alternative, problematic if you want to run from another pwd.
  #     :file => "locale/%s.yaml"
end

class MainController < Ramaze::Controller
  def index
    # Enclose the strings that have to be localized with [[]]
    # This works with any templating engine.
    "<h1>[[hello world]]</h1>
     <p>[[just for fun]]</p>
     <a href='/locale/en'>[[English]]</a><br />
     <a href='/locale/de'>[[German]]</a><br />
    "
  end

  def locale(name)
    session[:LOCALE] = name
  end
end

Ramaze.start
