*maglev-runtime
rescue2: anExceptionClass do: handlerBlock else: elseBlock
  "invoked from generated code"
  | res noExcepts |
  res := [ | blkRes | 
          blkRes := self @ruby2:value .
          noExcepts := true  .
          blkRes
        ] rescue2: anExceptionClass do: handlerBlock.
  noExcepts ifNil:[ ^ res ].
  ^ elseBlock @ruby2:value .

