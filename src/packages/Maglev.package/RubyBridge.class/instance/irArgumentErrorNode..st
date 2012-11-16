*maglev-runtime
irArgumentErrorNode: tooManyBool
  | send sel  |
  sel := tooManyBool ifTrue:[ #signalTooManyArgs ] ifFalse:[ #signalTooFewArgs ].
  ( send := GsComSendNode new)
         rcvr: ( GsComLiteralNode newObject: ArgumentError ) ;
         stSelector:   sel .
  ^ send

