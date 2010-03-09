# file trace_macros.m4
#
ifdef(`PARSER_DEBUG',` define(yTrace, `yDebugTrace( $1 )' ) ' 
       ,
                 `define(yTrace, `# $1 ') '
) 

# if PARSER_DEBUG not defined, 
#  generated action for Y_TRACE_val_vofs resolves to _reduce_noneOne 
ifdef(`PARSER_DEBUG', `
    define(Y_TRACE_val_vofs, ` {
yDebugTrace( $1 )
result = val[vofs] 
}')
  ' ,
  `  define(Y_TRACE_val_vofs, `')
  '
) 
# end file trace_macros.m4
