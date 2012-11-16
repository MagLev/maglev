*maglev-ast
_new: argOne with: argTwo
| res |
res := self _basicNew .
res list: { argOne . argTwo } .
^ res

