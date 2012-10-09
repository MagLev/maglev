*maglev-runtime
_validateInstance: anObject

   anObject ifNotNil:[
    anObject class == MatchData ifFalse:[ ArgumentTypeError signal:'expected a MatchData'].
   ].
   ^ anObject

