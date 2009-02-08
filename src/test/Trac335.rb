# Distilled from rubygems-1.3.1
#
module Gem
  class LoadError < ::LoadError
    attr_accessor :name, :version_requirement
  end
end

class Gem::ConfigFile
  Y = 99
  x =
    begin
      require 'Win32API'  # A file that should not exist
      'FAILURE'
    rescue LoadError   # should bind to ::LoadError
      'SUCCESS'
    end
  unless x == 'SUCCESS' ;  raise 'Error' ; end
end
true
