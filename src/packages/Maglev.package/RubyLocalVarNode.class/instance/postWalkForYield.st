*maglev-runtime
postWalkForYield
  | utp |
  useToProc ifNil:[ useToProc := 0 ]
         ifNotNil:[ useToProc := (utp := useToProc) + utp 
                    "use of incoming blk does not need to_proc" ].
  location varInfo readCount: -1  .

