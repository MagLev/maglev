as yet unclassified
_value
  isDefined ifNil:[ NameError signal:'uninitialized class variable ',key].
  ^ value

