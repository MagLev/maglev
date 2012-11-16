*maglev-runtime
rescue1: anExceptionClass do: handlerBlock else: elseBlock
  "invoked from generated code"
  | res noExcepts |
  res := [ | blkRes | 
          blkRes := self @ruby1:value .
          noExcepts := true  .
          blkRes
        ] rescue1: anExceptionClass do: handlerBlock.
  noExcepts ifNil:[ ^ res ].
  ^ elseBlock @ruby1:value .

