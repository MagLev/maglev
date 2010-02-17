
class Hash # class created in Smalltalk
  RemoteNil = _resolve_smalltalk_global(:_remoteNil) # TODO, would like constant private,
                # maybe remove this constant at end of bootstrap
                # or make remoteNil a literal like  nil but only accepted during bootstrap
  Fixnum_MAX = Fixnum::MAX
end
Hash.__freeze_constants

class IdentityHash  < Hash
  RemoteNil = _resolve_smalltalk_global(:_remoteNil) # TODO, would like constant private,
  Fixnum_MAX = Fixnum::MAX
end
IdentityHash.__freeze_constants

class Env  # created in smalltalk as RubyEnv
  RemoteNil = _resolve_smalltalk_global(:_remoteNil)
end
Env.__freeze_constants

