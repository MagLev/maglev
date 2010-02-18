
class CF
  def self.foo
    987
  end
end
cf = CF
CF.instance_eval { alias zfoo foo }
CF.instance_eval( "alias original_include? include? ") 
unless (rx = CF.zfoo ) == 987 ; raise 'error'; end
true
