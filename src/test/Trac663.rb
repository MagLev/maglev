
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
#################### Trac Info
# ID:         663
# Summary:    Klass.instance_eval { alias new_name old_name } blows up.
# Changetime: 2010-02-18 18:06:00+00:00
###

#  ruby -v 
#  # => ruby 1.8.7 (2008-08-11 patchlevel 72) [universal-darwin10.0]
#  
#  maglev -v 
#  # => maglev 0.6 (ruby 1.8.6) (2010-02-02 rev 22816-1116) [Darwin i386]
#  
#  class Foo; end
#  Foo.instance_eval { alias original_include? include? }
#  # => NameError: alias_method:  no method found for include?
#  
#  Makes alias_method_chain-ing slightly harder for class methods. Example doesn't crash in MRI. Luckily, 
#  
#  class << Foo; alias original_include? include?; end
#  
#  does work.