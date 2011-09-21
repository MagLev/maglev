unless @noload
  @noload = true
  eval(File.read(File.expand_path("../Trac943.rb", __FILE__)))
else
  require File.expand_path("../Trac943.rb", __FILE__)
end
