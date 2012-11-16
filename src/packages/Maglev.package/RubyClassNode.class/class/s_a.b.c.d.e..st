*maglev-runtime
s_a: aCpath b: aSuperNode c: aBody d: fileSourceString e: srcOfs
  | res |
  (res := self _basicNew )
     lineBias: 0  ;
     cpath: aCpath asClassNameNode ;
     superNode: aSuperNode ; position: srcOfs ;
     bodyNode: aBody source: fileSourceString .
  ^ res

