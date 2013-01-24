*maglev-runtime
copyRubyCHeaderFiles
 | dest src srcF destF |
 dest := '$MAGLEV_HOME/lib/ruby/1.9/include/ruby.h' .
 src :=  '$MAGLEV_HOME/../svn/src/ruby.h' .
 (srcF := GsFile openReadOnServer: src ) ifNotNil:[
   "we have a private build with svn checkout"
   GsFile removeServerFile: dest . "ignore errors"
   (destF := GsFile openWriteOnServer: dest) ifNil:[ self error:'create of ruby.h failed'].
   destF nextPutAll:'// Generated file, do not edit. master copy in svn
'.
   (destF nextPutAll: srcF contents ) ifNil:[ self error:'write to ruby.h failed'].
   (destF close ) ifNil:[ self error:'close of ruby.h failed.'].
   srcF close .
 ].

