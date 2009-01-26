require 'spec/helper'

spec_require 'mongrel'

def ramaze_options
  { :adapter => :mongrel }
end

require 'spec/ramaze/request'
