*maglev-runtime
_childScopeAt: aName
  |  sym ns assoc |
  sym := aName asSymbol .
  assoc := self resolveConstant: sym .
  assoc ifNotNil:[ ns := assoc _value ].  
  ns ifNotNil:[
    ns isBehavior ifTrue:[ | aCls |
       aCls := ns .
       ns := aCls transientNameSpaceForStore: envId .
    ].
  ].
  ^ ns

