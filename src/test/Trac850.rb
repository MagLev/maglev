# Found in i18n.rb.  The class defn works fine outside of the persistent block.
# The error is:
#
#  $ maglev-ruby ~/tmp/x.rb
#  error , a MessageNotUnderstood occurred (error 2010), a UndefinedObject does not understand  #'persistentCopy',
#               during /Users/pmclain/tmp/x.rb
#  ERROR 2010 , a MessageNotUnderstood occurred (error 2010), a UndefinedObject does not understand  #'persistentCopy' (MessageNotUnderstood)

Maglev.persistent do
  class C
    include Module.new {
      def foo
        puts "hi"
      end
    }
  end
end
C.new.foo
