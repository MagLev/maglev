*maglev-ast
bridgeForPrimAllowed: rubySel 
      "ruby_selector_suffix dependent"
   | firstChar |
   firstChar :=  rubySel at: 1 .
   firstChar == $s ifTrue:[
     (rubySel at:1 equals:'send') ifTrue:[
      rubySel == #'send#1*&' ifFalse:[ ^ false  "no bridge for    def send ; end  " ] ].
   ].
   firstChar == $_ ifTrue:[
     (rubySel at:1 equals: '__send__')  ifTrue:[
      rubySel == #'__send__#1*&' ifFalse:[ ^ false  "no bridge for    def send ; end  " ] ].
   ].
   ^ true

