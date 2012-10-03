ruby-support
require: requireName
  "Ensure RubyGems is loaded, then load the ruby gem named gemName"
  self runRubySource: 'require "', requireName, '"'

