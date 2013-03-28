*maglev-runtime
_rubyInspect
  "a ruby primitive"
  | str names ts envId |
  envId := 1"__callerEnvId" .
  str := '#<' copy .
  (self isClass and: [self isRubySingletonClass or: [self isMeta]])
    ifTrue: [
      str addAll: self _rubyInspectSingletonClass.
      str addAll: $>]
    ifFalse: [
      str addAll: (self class rubyFullName: envId)  .
      ts := GsProcess _recursionGuardSet .
      (ts _addIfAbsent: self) ifFalse:[  "already in ts"
        str addAll: '...>' .  
       ^ str 
      ].
      [ | s |
        s := str .
        s addAll: ':0x'.
        s addAll: self asOop hex.
        names := self  rubyInstvarNames . 
        1 to: names size do:[:n | | nam |
          s add: $  .
          s addAll: (nam := names at: n)  ; add: $=  .
          s addAll: ((self rubyInstvarAt: nam asSymbol ) @ruby1:inspect ). 
        ].
        s add: $> . 
      ] ensure:[
        ts remove: self
      ]].
  ^ str

