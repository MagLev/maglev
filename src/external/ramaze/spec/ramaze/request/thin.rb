require 'spec/helper'

spec_require 'thin'

def ramaze_options
  { :adapter => :thin }
end

require 'spec/ramaze/request'
