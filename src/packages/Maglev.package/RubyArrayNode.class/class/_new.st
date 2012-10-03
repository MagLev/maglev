*maglev-ast
_new
| res |
res := self _basicNew .
res list: #() .  "note #() is canonicalized across image"
^ res

