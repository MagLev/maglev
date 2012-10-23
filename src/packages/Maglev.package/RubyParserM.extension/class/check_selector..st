*maglev-ast
check_selector: selTok
   " returns selector symbol , guards against malformed elsif"
  | sel |
  sel := selTok symval .
  (sel at: 1) == $e ifTrue:[ 
    " guard against malformed   elsif"
    (sel == #elseif or:[ sel == #elif]) ifTrue:[  
      " disallow the other forms of elsif common in script languages"
      self signalError: 'malformed elsif, found ', sel .
    ] ifFalse:[
      (sel at:1 equals: 'else') ifTrue:[
        self signalWarning: 'possible malformed elsif, found ', sel .
  ] ] ].
  ^ sel

