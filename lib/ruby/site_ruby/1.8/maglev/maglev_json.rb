# The json_pure gem currently needs a few patches to get it to run with
# MagLev.  This file should be loaded after the json_pure gem is loaded.
#
# See Trac #616 for more details.
#
require 'rubygems'
require 'iconv'
require 'json'

class Boolean
  alias to_json to_s
end
class Symbol
  def to_json
    "\"#{self}\""
  end
end
