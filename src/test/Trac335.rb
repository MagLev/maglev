# Distilled from rubygems-1.3.1
#
# If you comment out the definition of Gem::LoadError, then the script runs
# fine.  But with Gem::LoadError, the rescue clause does not get triggered.
module Gem
  class LoadError < ::LoadError
    attr_accessor :name, :version_requirement
  end
end

class Gem::ConfigFile
  system_config_path =
    begin
      require 'Win32API'  # A file that should not exist
      'FAILURE'
    rescue LoadError
      'SUCCESS'
    end
  puts system_config_path  # should be SUCCESS
end

