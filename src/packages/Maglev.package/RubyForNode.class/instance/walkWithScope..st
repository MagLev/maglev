*maglev-runtime
walkWithScope: aScope
   | itr inline |
    (itr := iterNode) walkIterWithScope: aScope .
    varNode walkWithScope: aScope .
    inline := itr irForNodeWillBeInline. 
    bodyInline := inline .
    inline ifFalse:[
      varNode setIsBlockArg
    ].
    bodyNode walkWithScope: aScope

