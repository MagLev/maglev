var_name = "RC_FOO"
code = 414
err_name = "AN_ERROR"
parent = "Object"
cl = eval %-
  RC_#{var_name} = #{code}
  class #{err_name} < #{parent}
    def self.code() RC_#{var_name} end
    def self.reason_phrase() StatusMessage[code] end
    def code() self::class::code end
    def reason_phrase() self::class::reason_phrase end
    alias to_i code
  end
  -

unless cl.name == 'AN_ERROR' ; raise 'Error' ; end
true
#################### Trac Info
# ID:         276
# Summary:    Can't define class via eval  ; broken by $~ fixes
# Changetime: 2008-12-12 20:38:37+00:00
###

#  from webrick/httpstatus.rb:
#  
#  
#  {{{
#  var_name = "RC_FOO"
#  code = 414
#  err_name = "AN_ERROR"
#  parent = "Object"
#  eval %-
#  #  RC_#{var_name} = #{code}
#    class #{err_name} < #{parent}
#      def self.code() RC_#{var_name} end
#      def self.reason_phrase() StatusMessage[code] end
#      def code() self::class::code end
#      def reason_phrase() self::class::reason_phrase end
#      alias to_i code
#    end
#    -
#  
#  
#  }}}
#  
#  The error is:
#  
#  
#  {{{
#  $ maglev-ruby src/test/Trac276.rb 
#  topaz 1> eval:6: class definition in method body
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  Error, 'error from parse server'
#  Error Category: [GemStone] Number: 2023 Arg Count: 1
#  Arg 1: error from parse server
#  topaz 1> 
#  [pmclain@pspace-2 git (dev)]$ 
#  
#  }}}
#  