(as yet unclassified)
irNode
  | ary lst |
  ary := GsComArrayBuilderNode new.
  lst := list .
  1 to: lst size do:[:n |
    ary appendElement: ( lst at: n) irNode
  ].
  self ir: ary.
  ^ ary

