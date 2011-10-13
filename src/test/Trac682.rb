# From DataMapper
#
# The class eval fails when the property_option is :format, but works ok
# when the property_option is :length
#
# The error is:
#
#  $ mruby $pbm
#  error ,  in class aMetaModule instVar format is private to Smalltalk,
#            during /Users/pmclain/GemStone/dev/pbm.rb
#  ERROR 2023, Error, ' in class aMetaModule instVar format is private to Smalltalk' (SyntaxError)

class Type
  PROPERTY_OPTIONS = [:length, :format ]
  class << self
    PROPERTY_OPTIONS.each do |property_option|
      self.class_eval <<-RUBY, __FILE__, __LINE__ + 1
        def #{property_option}(*args)
          if args.any?
            @#{property_option} = args.first
          else
            defined?(@#{property_option}) ? @#{property_option} : nil
          end
        end
      RUBY
    end
  end
end


# Another instance from Rails:
class Railtie
  class << self
    def subclasses
      @subclasses ||= []
    end
  end
end

#################### Trac Info
# ID:         682
# Summary:    MagLev has trouble eval method def named 'format' - name clash with Smalltalk IV names
# Changetime: 2010-04-07 16:59:33+00:00
###

#  From DataMapper:
#  
#  The eval of method def :length works, but the eval of method def :format fails (probably clash with Behavior>>format):
#  
#  {{{
#  
#  class Type
#    PROPERTY_OPTIONS = [:length, :format ]
#    class << self
#      PROPERTY_OPTIONS.each do |property_option|
#        self.class_eval <<-RUBY, __FILE__, __LINE__ + 1
#          def #{property_option}(*args)
#            if args.any?
#              @#{property_option} = args.first
#            else
#              defined?(@#{property_option}) ? @#{property_option} : nil
#            end
#          end
#        RUBY
#      end
#    end
#  end
#  
#  }}}
#  
#  The Error:
#  
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb
#  error ,  in class aMetaModule instVar format is private to Smalltalk,
#            during /Users/pmclain/GemStone/checkouts/git/src/test/TracXXX.rb
#  ERROR 2023, Error, ' in class aMetaModule instVar format is private to Smalltalk' (SyntaxError)
#  
#  }}}
#  
#  