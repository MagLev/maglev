# Extracted from Rails 3
#
# MagLev gives the following error:
#   $ mruby $pbm
#   XXX UrlHelpers.with(ARoute)
#   XXX ApplicationController.inherited(SayController)
#   #<NoMethodError: NoMethodError: undefined method `with' for ApplicationController>`with' called
#   /Users/pmclain/GemStone/dev/pbm.rb:11:in `method_missing'
#   /Users/pmclain/GemStone/dev/pbm.rb:11:in `with'
#   /Users/pmclain/GemStone/dev/pbm.rb:77
#   ERROR 2010, NoMethodError: undefined method `with' for ApplicationController (NoMethodError)
#

module AModule
end

module UrlHelpers
  def self.with(routes)
    Module.new do
      define_method(:inherited) do |klass|
        # When MagLev looks for super, it is trying to find with:&, not inherited:&
        # and we get the error
        super(klass)   
      end
    end
  end
end

class ApplicationController
end

class ARoute
  def self.url_helpers
    AModule
  end
end

ApplicationController.instance_eval do
  extend UrlHelpers.with(ARoute)
end

class SayController < ApplicationController
end
