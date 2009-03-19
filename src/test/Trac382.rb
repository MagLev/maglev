module Sinatra
  DEBUG_MRI = false  # true for printing object ids with MRI
  class Base
    def self.kick_off(&block)
      xx = self  # self is Application
      instance_eval(&block)
    end
  end

  class Application < Base
  end
end

a = Sinatra::Application.new
if Sinatra::DEBUG_MRI
  $sinatra_base_id = Sinatra::Base.__id__
  $sinatra_application_id = Sinatra::Application.__id__
  $sinatra_app_id = a.__id__
end

$sinatra_base = Sinatra::Base
$sinatra_application = Sinatra::Application
$sinatra_app = a

if Sinatra::DEBUG_MRI
  puts "----------------------------------------------------------------------"
  puts "Sinatra::Base: #{Sinatra::Base.__id__}   #{$sinatra_base_id}"
  puts "Sinatra::Application: #{Sinatra::Application.__id__}  #{$sinatra_application_id}"
  puts "A Sinatra::Application instance: #{a.__id__}  #{$sinatra_app_id}"
  puts "----------------------------------------------------------------------"
end

yy = self

Sinatra::Application.kick_off {
  # puts "In KickOff Block"
  v = self   # got Application class
  expv = $sinatra_application
  raise 'error' unless v.equal?(expv)
  puts "KickOff Block ok"
  #if Sinatra::DEBUG_MRI
  #  raise "kick_off block: expecting Sinatra::Application (#{$sinatra_application_id}) but got: #{self} #{__id__}" unless $sinatra_application_id == __id__
  #end
}
