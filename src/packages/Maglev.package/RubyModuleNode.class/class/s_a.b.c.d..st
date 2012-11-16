*maglev-runtime
s_a: aCpath b: aBody c: fileSourceString d: srcOfs
  | res |
  (res := self _basicNew )
     lineBias: 0  ;
     cpath: aCpath asClassNameNode ; position: srcOfs; 
     bodyNode: aBody source: fileSourceString .
  ^ res

