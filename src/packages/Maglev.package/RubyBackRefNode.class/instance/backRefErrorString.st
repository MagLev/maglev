*maglev-ast
backRefErrorString
  | sel | 
  (sel := selector) == #pre_match ifTrue:[ ^ '$`' ].
  sel == #post_match ifTrue:[ ^ '$'''].
  sel == #__plus_match ifTrue:[ ^ '$+'].

