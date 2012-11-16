as yet unclassified
argsDescrInt
  "Bits within the SmallInteger describing method defintion args 
     as built by buildMethodArgumentsOn: 
         16rFF  number of Args excluding  * and & args
       16rFF00  one-based number of first optional arg, or zero of none
      16r10000   one if has * arg 
     16r100000   one if has & arg "

  ^ argsDescrInt

