ruby-support
loadGem: gemName
  "Ensure RubyGems is loaded, then load the ruby gem named gemName"
  self runRubySource: 'require "rubygems"; require "', gemName, '"' .

