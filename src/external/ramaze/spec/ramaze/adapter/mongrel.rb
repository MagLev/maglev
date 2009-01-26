#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

spec_require 'mongrel'

def ramaze_options
  { :adapter => :mongrel }
end

require 'spec/ramaze/adapter'
