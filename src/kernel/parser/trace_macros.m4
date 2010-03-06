# file trace_macros.m4
#
ifdef(`PARSER_DEBUG',
  ` define(yTrace, `yDebugTrace( $1 )' ) ' ,
  ` define(yTrace, `# $1 ') '
) 
# end file trace_macros.m4
