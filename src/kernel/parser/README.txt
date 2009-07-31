This file discusses usage of the ruby parser, which was
derived from Ryan Davis'  ruby_parser2.0.3
For implementation notes, see  implementation.txt .

The native ruby parser is now the default.

To load the ruby parser,  use topaz to load the file 
   kernel/parser/loadrp.inp .
After you have loaded the primitives .

To switch back to the the old MRI parser , you must
execute this following smalltalk after login and before
execution of ruby code:

  SessionTemps current at: #MAGLEV_evalUseMri put: true ;
                       at: #MAGLEV_loadUseMri put: true .

Or by using the following Ruby code

  Gemstone.session_temp_put( :MAGLEV_evalUseMri , true )
  Gemstone.session_temp_put( :MAGLEV_loadUseMri , true )

MAGLEV_evalUseMri and MAGLEV_loadUseMri can be set true or false
independently and at any time and will control which parser is
used for next eval or load/require .  MAGLEV_loadUseMri
is checked at start of processing a file for a load or a require.

MagRpDEBUG values
   0 means no logging, 
   1 means print each file parsed by the ruby parser,  
   2 means include lexer and parser debug output.

Smalltalk example:
   SessionTemps current at: #MagRpDEBUG put: 1 .
Ruby example
   Gemstone.session_temp_put( :MagRpDEBUG , 1 )

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
