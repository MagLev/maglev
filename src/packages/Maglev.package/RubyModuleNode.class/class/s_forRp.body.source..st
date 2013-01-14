*maglev-runtime
s_forRp: aCpath body: aBody source: fileSourceString 
  | res |
  (res := self _basicNew )
     lineBias: 0  ;
     cpath: aCpath asClassNameNode ; 
     bodyNode: aBody source: fileSourceString .
  ^ res

