*maglev-runtime
defineClassNamed: aSymbol rubyMethod: aNode inScope: aParent superclass: superCls
    fixedIvs: ivList

  "called from generated code"
   ^ self defineClassNamed: aSymbol rubyMethod: aNode inScope: aParent
      superclass: superCls env: 1"__callerEnvId" fixedIvs: ivList

