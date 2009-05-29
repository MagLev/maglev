To load the ruby parser,  use topaz to load the file 
   kernel/parser/loadrp.inp .
After you have loaded the primitives .

To enable use of the ruby parser , you must
execute this following smalltalk after login and before
execution of ruby code:

  SessionTemps current at: #MAGLEV_evalUseRp put: true ;
                       at: #MAGLEV_loadUseRp put: true ;
     at: #MAGLEV_logSexp put: false ;
     at: #MagRpDEBUG put: 1 .

Or by using the following Ruby code

  Gemstone.session_temp_put( :MAGLEV_evalUseRp , true )
  Gemstone.session_temp_put( :MAGLEV_loadUseRp , true )
  Gemstone.session_temp_put( :MAGLEV_logSexp ,   false )
  Gemstone.session_temp_put( :MagRpDEBUG , 1 )

MAGLEV_evalUseRp and MAGLEV_loadUseRp can be set true or false
independently and at any time and will control which parser is
used for next eval or load/require .  MAGLEV_loadUseRp
is checked at start of processing a file for a load or a require.

MagRpDEBUG values
   0 means no logging, 
   1 means print each file parsed by the ruby parser,  
   2 means include lexer and parser debug output.

If you edit the file kernel/parser/ruby_parser.y
and want your edits to be used, you must
   cd kernel/parser
   ./racc.sh 
to regenerate the file  kernel/parser/ruby_parser.rb ,
and then reload the primitives via
  RubyContext reset ; load 
and then reload the ruby parser with kernel/parser/loadrp.inp .
The script  racc.sh  does additional processing on the output
from racc gem using sed  . Do not run the racc  gem  directly  .
