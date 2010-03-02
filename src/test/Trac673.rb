# From Rails3 ActiveSupport
#
# $ mruby $pbm
#  error , User defined error, 'irNode not yet implemented for RubyKBeginNode , near unknown source position',
#          during /Users/pmclain/GemStone/dev/pbm.rb
# ERROR 2318, User defined error, 'irNode not yet implemented for RubyKBeginNode , near unknown source position' (UserDefinedError)
#
#

module ClassMethods
  def __create_keyed_callback(name, kind, object, &blk) #:nodoc:
    @_keyed_callbacks[name] ||= begin
      str = send("_#{kind}_callbacks").compile(name, object)
    end
  end
end
