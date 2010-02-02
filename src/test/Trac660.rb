# From Rack 1.1.0
module Rack
  class File
  end
  class Builder
    def self.parse_file
      config = "some_file.rb"
      cfgfile = "$:.unshift File.dirname(__FILE__) + '/lib'"
      app = eval "Rack::Builder.new {( " + cfgfile + "\n )}",
        TOPLEVEL_BINDING, config
    end
    def initialize(&block)
      instance_eval(&block)
    end
  end
end

Rack::Builder.parse_file
