module Sinatra
  class Base
    class << self

      def kick_off(&block)
        raise "Bad self in :kick_off: #{self} #{__id__}" unless $sinatra_application_id == __id__
        instance_eval(&block)
      end

      # This defines a method and stores the proc in a hash
      def set(method_name, a_proc)
        @@a_proc ||= { }
        @@a_proc[method_name] = a_proc
        metadef(method_name, &a_proc)
      end

      # Retrieve the procs used to define methods
      def get_a_proc(key)
        @@a_proc[key]
      end

      def metadef(message, &block)
        (class << self; self; end).
          send :define_method, message, &block
      end
    end  # end class << self

    # The following calls to set will define methods that share the same home context.
    # Then we instance_eval and/or call them in various ways.
    set :aaa, Proc.new {
      raise ":aaa, start: self was #{__id__}" unless __id__ == $sinatra_application_id
      some_proc = get_a_proc :bbb
      get_instance.instance_eval(&some_proc)
      raise ":aaa, end: self was #{__id__}" unless __id__ == $sinatra_application_id
    }

    set :bbb, Proc.new {
      raise ":bbb, start: self was #{__id__}" unless __id__ == $sinatra_app_id
      self.class.ccc
    }

    set :ccc, Proc.new {
      raise ":ccc, start: self was #{__id__}" unless __id__ == $sinatra_application_id
    }
  end

  class Application < Base
    # Create a singleton instance and return it
    def self.get_instance
      @@instance ||= self.new
      @@instance
    end
  end
end

a = Sinatra::Application.get_instance
raise 'get_instance failing' unless a.equal?(Sinatra::Application.get_instance)

$sinatra_base_id = Sinatra::Base.__id__
$sinatra_application_id = Sinatra::Application.__id__
$sinatra_app_id = a.__id__

# puts "----------------------------------------------------------------------"
# puts "Sinatra::Base:                   #{Sinatra::Base.__id__}  #{$sinatra_base_id}"
# puts "Sinatra::Application:            #{Sinatra::Application.__id__}  #{$sinatra_application_id}"
# puts "A Sinatra::Application instance: #{a.__id__}  #{$sinatra_app_id}"
# puts "----------------------------------------------------------------------"

Sinatra::Application.kick_off {
  raise "Bad self in :kick_off block: #{self} #{__id__}" unless $sinatra_application_id == __id__

  # The instance_eval of the block in kick_off has set self to
  # Sinatra::Application.  We are currently in an instance_eval, and now we
  # want to do another but set self to an application object.
  a_proc = get_a_proc :aaa
  instance_eval(&a_proc)
}
