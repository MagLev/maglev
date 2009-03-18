module Sinatra
  class Base
    def self.kick_off(&block)
      raise "Base.kick_off: expecting Sinatra::Application (#{$sinatra_application_id}) but got: #{self} #{__id__}" unless $sinatra_application_id == __id__
      puts "In kickoff"
      instance_eval(&block)
    end
  end

  class Application < Base
  end
end

a = Sinatra::Application.new
$sinatra_base_id = Sinatra::Base.__id__
$sinatra_application_id = Sinatra::Application.__id__
$sinatra_app_id = a.__id__

puts "----------------------------------------------------------------------"
puts "Sinatra::Base: #{Sinatra::Base.__id__}   #{$sinatra_base_id}"
puts "Sinatra::Application: #{Sinatra::Application.__id__}  #{$sinatra_application_id}"
puts "A Sinatra::Application instance: #{a.__id__}  #{$sinatra_app_id}"
puts "----------------------------------------------------------------------"

Sinatra::Application.kick_off {
  puts "In KickOff Block"
  raise "kick_off block: expecting Sinatra::Application (#{$sinatra_application_id}) but got: #{self} #{__id__}" unless $sinatra_application_id == __id__
}
