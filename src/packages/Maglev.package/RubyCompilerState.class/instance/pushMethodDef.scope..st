*maglev-runtime
pushMethodDef: methodDefNode scope: aScope
  "returns true if previous top of stack is a RubyMethodDefNode,
    or we are within an eval"
   | methStk prev |
   methStk := methStack.
   prev := methStk topOrNil .
   methStk push: methodDefNode .
   loopStack push: nil .
   prev isMethodDef ifTrue:[
     "setup the ref from an inner def to the outermost def"
     | outerMost |
     methodDefNode outerDef: (outerMost := prev outerDef) . "the outer-most def"
     outerMost setHasInnerEvalOrDef .
     ^ true
   ].
   (evalLexicalSelfStack size > 0 and:[ prev walkInEval ]) ifTrue:[ 
     "In an eval, and not within class or module within eval"
     methodDefNode outerDef: methodDefNode .
     ^ true
   ].
   ^ false

