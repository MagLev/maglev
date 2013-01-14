*maglev-ast
literal_concat: headArg tail: tailArg
  ".y file checks for head/tail nil"
  | htype ttype head tail |
  htype := (head := headArg) str_dstr_evstr_kind . " 0,1,2  or nil"
  ttype := (tail := tailArg) str_dstr_evstr_kind .
  htype == 2 ifTrue:[
    head := RubyDStrNode s_a: { RubyStrNode s_a: String new .  head } 
  ].
  ttype == 0 ifTrue:[
    htype == 0 ifTrue:[
      " append tail's String to head's String"
      head appendString: tail strNodeValue 
    ] ifFalse:[
      (htype == 1 and:[ head size == 1]) ifTrue:[
        "head is a dstr with a StrNode and no  further list elements"
        head appendToHeadString: tail strNodeValue .
      ] ifFalse:[
        "htype == :dstr , or was :evstr changed to :dstr above"
        head appendToList: tail 
    ] ] ] ifFalse:[
  ttype == 1 ifTrue:[
    htype == 0 ifTrue:[
      tail appendToHeadString: head strNodeValue .
      head := tail
    ] ifFalse:[ | t_list t_list_head | "tail and head both  :dstr"
      t_list := tail dstrList .
      t_list_head := t_list atOrNil: 1 .
      t_list_head strNodeValue = '' ifTrue:[
        t_list removeFrom: 1 to: 1 .
      ].
      head dstrList addAll: t_list
    ] ] ifFalse:[
  ttype == 2 ifTrue:[  "tail is EvStrNode"
    htype == 0 ifTrue:[ " head is StrNode" | t_body |
      t_body := tail evStrBody .
      t_body class == RubyStrNode ifTrue:[
    head appendString: t_body strNodeValue .
      ] ifFalse:[ | dstr |
        head := RubyDStrNode s_a:{ head . tail } .
      ]
    ] ifFalse:[ " head is a DStrNode"
      head dstrList add: tail 
    ]
  ] ifFalse:[
    self signalError: 'literal_concat unknown type' 
  ] ] ] .
  ^ head

