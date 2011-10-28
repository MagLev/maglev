
set class RubyVarLocation
category: '*maglev-runtime'
method:
depth
  ^ depth

%


set class RubyVarLocation
category: '*maglev-runtime'
method:
leaf
  ^ info leafInScope: scope

%


set class RubyVarLocation
category: '*maglev-runtime'
method:
varInfo
  "result is a RubyScopeVarInfo"
  ^ info

%


set class RubyVarLocation
category: '*maglev-runtime'
method:
varInfo: aRubyScopeVarInfo depth: anInt scope: aScope
  info := aRubyScopeVarInfo .
  depth := anInt  .
  scope := aScope .
  ^ self

%

