*maglev-runtime
comment
^ '
  instVars
    parent - a RubyNameSpace , or nil for the top scope
    name   - aString, for debugging
    myClass - the Class for which cls.rubyNameSpace==thisRubyNameSpace
        the RubySymbolAssociation key==self.name , value=aClassOrModule
                is in the parent scope
    assocInParent - the RubySymbolAssociation in the parent scope such that
         assoc.value.rubyNameSpace == self    (for a Module or class) or
         assoc.value == self                    (for Module compiled but not defined? yet)

    deferList - an Array (non-nil only in the root scope)
			
    methodProtection  0 - none, 1 protected, 2 private ,
       captures most recent zero-arg  private or protected directive for myClass
			
'

