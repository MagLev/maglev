
  fileStack replaces RubyCurrentFile .

  scopeStack replaces RubyCurrentScope . typically scopes are created during
     walkWithScope phase  and captured in method, class, or block nodes of the AST graph .
     The scope stack is used during both walkWithScopes phase and IR generation phase.

  lexLevel replaces RubyCurrentLexLevel

  compilerStack  replaces RubyCurrentCompiler
  methStack  is a stack of RubyMethDefNodes, used during the walkWithScope phase 

  loopStack is used to handle RubyWhileNode and RubyUntilNode during walkWithScopes 
    and IR phases.  The top element of the loopStack will be a RubyNode during processing
    of an in-line loop's block, and will be nil during processing other blocks.

  rtModuleStack is the runtime stack of nested classes/modules.
    also accessible as (GsProcess _current _rubyThreadDataAt: 5)
