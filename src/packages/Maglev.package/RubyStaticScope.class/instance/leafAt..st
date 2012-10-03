*maglev-runtime
leafAt: aRubyVarLocation 
   "creates a VarLeaf if needed."
   | depth |
   depth := aRubyVarLocation depth. 
  ^ (self scopeAt: depth) leafForVarInfo: aRubyVarLocation varInfo .

