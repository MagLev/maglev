*maglev-runtime
_addVarInfo: infoClass name: nam kind: kindSym
   "infoClass is RubyScopeVarInfo or subclass thereof"
    | vnams ofs vInfo |
  vnams := variableNames .
  ofs := vnams size + 1 .
  vInfo := infoClass new: nam kind: kindSym ofs: ofs .
  vnams at: ofs put:  vInfo .
  ^ vInfo

