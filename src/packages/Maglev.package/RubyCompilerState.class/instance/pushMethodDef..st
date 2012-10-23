*maglev-runtime
pushMethodDef: methOrClassBody
  "returns self"
   methStack push: methOrClassBody .
   loopStack push: nil .

