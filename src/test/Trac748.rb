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

unless defined?(Maglev)
  class NilClass
    def pause
    end
  end
end

module UrlHelpers
  def self.with(routes)
    Module.new do
      sx = self  # sx [121237761  meta] ; self [121237761  meta]
      #nil.pause
      define_method(:inherited) do |klass|
        # When MagLev looks for super, it is trying to find with:&, not inherited:&
        # and we get the error
        puts 'In defined inherted'
        puts "self is #{self}"
        kx = klass
        super(kx)   
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
  mx = UrlHelpers.with(ARoute)
  #nil.pause
  p self.ancestors
  extend(mx)
  sx = self  # sx [121240577  ApplicationController class]
  #nil.pause
  p self.ancestors
end

class SayController < ApplicationController
end
