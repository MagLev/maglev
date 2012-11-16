*maglev-runtime
c3name: aSymbol
  "used by RubyColon3Node "
  lexPathSize := 1 .
  self size: 3 ;
       at: 1 + 2 put: aSymbol ;
       at: 1 + 1 put: nil "evalSelfCls" ;
       at: 1 put: Object .
  "globalAssoc left as nil" 

