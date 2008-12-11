var_name = "RC_FOO"
code = 414
err_name = "AN_ERROR"
parent = "Object"
eval %-
#  RC_#{var_name} = #{code}
  class #{err_name} < #{parent}
    def self.code() RC_#{var_name} end
    def self.reason_phrase() StatusMessage[code] end
    def code() self::class::code end
    def reason_phrase() self::class::reason_phrase end
    alias to_i code
  end
  -

