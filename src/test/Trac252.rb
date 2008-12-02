module Gem
  ConfigMap = { }
  require 'rbconfig'
  RbConfig = Config unless defined? ::RbConfig
  ConfigMap.merge!(:BASERUBY => RbConfig::CONFIG["BASERUBY"])
end
true
