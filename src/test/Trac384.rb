module Sinatra
  FOR_MRI=false  # change to true to get MRI object id printing
  class Base
    class << self

      def kick_off(&block)
        if FOR_MRI
          raise "Bad self in :kick_off: #{self} #{__id__}" unless $sinatra_application_id == __id__
        else
          exp = $sinatra_application
          act = self
          raise "Bad self in :kick_off" unless act.equal?(exp)
        end
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
      if FOR_MRI
        raise ":aaa, start: self was #{__id__}" unless __id__ == $sinatra_application_id
      else
        exp = $sinatra_application
        act = self
        raise ":aaa, start: bad self " unless act.equal?(exp)
      end
      some_proc = get_a_proc :bbb
      get_instance.instance_eval(&some_proc)
      if FOR_MRI
        raise ":aaa, end: self was #{__id__}" unless __id__ == $sinatra_application_id
      else
        exp = $sinatra_application
        act = self
        raise ":aaa, end: bad self " unless act.equal?(exp)
      end
    }

    set :bbb, Proc.new {
      if FOR_MRI
        raise ":bbb, start: self was #{__id__}" unless __id__ == $sinatra_app_id
      else
        exp = $sinatra_app
        act = self
        raise ":bbb, start: bad self " unless act.equal?(exp)
      end
      self.class.ccc
    }

    ccdef_self = self  # ccdef_self [95892737  Base class]   Base
    set :ccc, Proc.new {
      if FOR_MRI
        raise ":ccc, start: self was #{__id__}" unless __id__ == $sinatra_application_id
      else
        exp = $sinatra_application # fails, act [95886337  Application]  anApplication
        act = self                 #         exp [95888385  Application class] Application
        raise ":ccc, start: self bad" unless act.equal?(exp)
      end
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

if Sinatra::FOR_MRI
  $sinatra_base_id = Sinatra::Base.__id__
  $sinatra_application_id = Sinatra::Application.__id__
  $sinatra_app_id = a.__id__
else
  $sinatra_base = Sinatra::Base
  $sinatra_application = Sinatra::Application
  $sinatra_app = a
end

# puts "----------------------------------------------------------------------"
# puts "Sinatra::Base:                   #{Sinatra::Base.__id__}  #{$sinatra_base_id}"
# puts "Sinatra::Application:            #{Sinatra::Application.__id__}  #{$sinatra_application_id}"
# puts "A Sinatra::Application instance: #{a.__id__}  #{$sinatra_app_id}"
# puts "----------------------------------------------------------------------"

Sinatra::Application.kick_off {
  if Sinatra::FOR_MRI
    raise "Bad self in :kick_off block: #{self} #{__id__}" unless $sinatra_application_id == __id__
  else
    exp = $sinatra_application
    act = self
    raise "Bad self in :kick_off block" unless act.equal?(exp)
  end

  # The instance_eval of the block in kick_off has set self to
  # Sinatra::Application.  We are currently in an instance_eval, and now we
  # want to do another but set self to an application object.
  a_proc = get_a_proc :aaa
  instance_eval(&a_proc)
}

puts "384 Passed"
true
#################### Trac Info
# ID:         384
# Summary:    more self is wrong in instance_eval ; need inew instVar access bytecodes
# Changetime: 2009-03-30 21:48:23+00:00
###

#  Distilled from Sinatra
#  
#  
#  {{{
#  module Sinatra
#    class Base
#      class << self
#  
#        def kick_off(&block)
#          raise "Bad self in :kick_off: #{self} #{__id__}" unless $sinatra_application_id == __id__
#          instance_eval(&block)
#        end
#  
#        # This defines a method and stores the proc in a hash
#        def set(method_name, a_proc)
#          @@a_proc ||= { }
#          @@a_proc[method_name] = a_proc
#          metadef(method_name, &a_proc)
#        end
#  
#        # Retrieve the procs used to define methods
#        def get_a_proc(key)
#          @@a_proc[key]
#        end
#  
#        def metadef(message, &block)
#          (class << self; self; end).
#            send :define_method, message, &block
#        end
#      end  # end class << self
#  
#      # The following calls to set will define methods that share the same home context.
#      # Then we instance_eval and/or call them in various ways.
#      set :aaa, Proc.new {
#        raise ":aaa, start: self was #{__id__}" unless __id__ == $sinatra_application_id
#        some_proc = get_a_proc :bbb
#        get_instance.instance_eval(&some_proc)
#        raise ":aaa, end: self was #{__id__}" unless __id__ == $sinatra_application_id
#      }
#  
#      set :bbb, Proc.new {
#        raise ":bbb, start: self was #{__id__}" unless __id__ == $sinatra_app_id
#        self.class.ccc
#      }
#  
#      set :ccc, Proc.new {
#        raise ":ccc, start: self was #{__id__}" unless __id__ == $sinatra_application_id
#      }
#    end
#  
#    class Application < Base
#      # Create a singleton instance and return it
#      def self.get_instance
#        @@instance ||= self.new
#        @@instance
#      end
#    end
#  end
#  
#  a = Sinatra::Application.get_instance
#  raise 'get_instance failing' unless a.equal?(Sinatra::Application.get_instance)
#  
#  $sinatra_base_id = Sinatra::Base.__id__
#  $sinatra_application_id = Sinatra::Application.__id__
#  $sinatra_app_id = a.__id__
#  
#  # puts "----------------------------------------------------------------------"
#  # puts "Sinatra::Base:                   #{Sinatra::Base.__id__}  #{$sinatra_base_id}"
#  # puts "Sinatra::Application:            #{Sinatra::Application.__id__}  #{$sinatra_application_id}"
#  # puts "A Sinatra::Application instance: #{a.__id__}  #{$sinatra_app_id}"
#  # puts "----------------------------------------------------------------------"
#  
#  Sinatra::Application.kick_off {
#    raise "Bad self in :kick_off block: #{self} #{__id__}" unless $sinatra_application_id == __id__
#  
#    # The instance_eval of the block in kick_off has set self to
#    # Sinatra::Application.  We are currently in an instance_eval, and now we
#    # want to do another but set self to an application object.
#    a_proc = get_a_proc :aaa
#    instance_eval(&a_proc)
#  }
#  
#  }}}
#  
#  The error is that self is wrong when "self.class.ccc" is called from the instance eval.  The self is the application instance, not the application class:
#  
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb 
#  error , :ccc, start: self was 94244865,
#            during /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  ERROR 2023, Error, ':ccc, start: self was 94244865'
#  
#  }}}
#  
#  