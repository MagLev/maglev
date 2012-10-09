*maglev-runtime
walkEvalCall: aScope
  "Returns a new lexPathVar "
  | cst |
  cst := RubyCompilerState current .
  cst topMethodDef  setSendsBinding ; setHasBlockArgRef ; setHasInnerEvalOrDef .
  ^ ( RubyVcGlobalNode _basicNew ) name: #'__lexPath' 

