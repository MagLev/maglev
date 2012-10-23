!=========================================================================
! Copyright (C) VMware, Inc. 2008.  All Rights Reserved.
!
! $Id: baseruby.gs 26114 2011-07-08 16:18:18Z stever $
!
! Name - baseruby.gs
!    top level filein input script for slowrubyimage step of the build
!========================================================================
set user SystemUser pass swordfish
iferr 1 exit
login

! iferr 1 where  
iferr 2 stack

output push baseruby.out 

run
"Ruby is not yet ready for Portable streams, nor Legacy streams with ANSI polarity"
Stream installLegacyStreamImplementation.
(Globals at: #'PositionableStream_position') == #'ANSI'
  ifTrue: [
    Globals at: #'PositionableStream_position' put: #'Legacy'.
    PositionableStream compilePositionMethods ].
true
%

set class PositionableStream
category: '*squeak'
method:
originalContents

 ^ itsCollection

%


input $imageRubyDir/GsCompilerClasses.gs

input $imageRubyDir/Behavior_ruby.gs

! class definitions
input $imageRubyDir/Classes_ruby.gs

input $imageRubyDir/Class_ruby.gs
input $imageRubyDir/Metaclass3_ruby.gs
input $imageRubyDir/Capi_ruby.gs
input $imageRubyDir/ObsoleteMetaclass_ruby.gs
input $imageRubyDir/UnorderedCollection_ruby.gs

! methods for GsCompilerIRNode classes
input $imageRubyDir/GsCompilerIRNode.gs
input $imageRubyDir/GsNMethod_ruby.gs

! Smalltalk env 0 and env1 methods for other base classes
input $imageRubyDir/Association_ruby.gs
input $imageRubyDir/Array_ruby.gs
input $imageRubyDir/CharacterCollection_ruby.gs
input $imageRubyDir/ExecBlock_ruby.gs
input $imageRubyDir/Exception_ruby.gs
input $imageRubyDir/IO_ruby.gs
input $imageRubyDir/GsFile_ruby.gs
input $imageRubyDir/GsProcess_ruby.gs
input $imageRubyDir/GsSocket_ruby.gs
input $imageRubyDir/Numeric.gs
input $imageRubyDir/MatchData.gs
input $imageRubyDir/NilTF.gs
input $imageRubyDir/Object_ruby.gs
input $imageRubyDir/ProcessorScheduler_ruby.gs
input $imageRubyDir/Repository_ruby.gs
input $imageRubyDir/Symbol_ruby.gs
input $imageRubyDir/SymbolAssociation_ruby.gs
input $imageRubyDir/String_ruby.gs
input $imageRubyDir/System_ruby.gs
input $imageRubyDir/Range_ruby.gs
input $imageRubyDir/Regexp.gs
input $imageRubyDir/RubyConstantRef.gs
input $imageRubyDir/RubyDirectory.gs
input $imageRubyDir/RubyEnv.gs
input $imageRubyDir/RubyProc.gs
input $imageRubyDir/RubyThreadGroup.gs
input $imageRubyDir/RubySocket.gs 
input $imageRubyDir/IPSocket.gs
input $imageRubyDir/TCPSocket.gs
input $imageRubyDir/TCPServer.gs
input $imageRubyDir/UDPSocket.gs
input $imageRubyDir/UNIXSocket.gs
input $imageRubyDir/UNIXServer.gs
input $imageRubyDir/TransientShortArray_ruby.gs
input $imageRubyDir/RubyHash.gs
input $imageRubyDir/RubyIdentityHash.gs
! DateTime_ruby.gs no longer used
input $imageRubyDir/VariableContext_ruby.gs
input $imageRubyDir/Module_ruby.gs
input $imageRubyDir/Kernel.gs
input $imageRubyDir/RubySignal.gs
input $imageRubyDir/RubyTime.gs
input $imageRubyDir/ProfMonitorTree_ruby.gs
input $imageRubyDir/ProfMonitorEntry_ruby.gs
input $imageRubyDir/GsObjectSecurityPolicies.gs

expectvalue true
run
| rpt f |
rpt := ClassOrganizer new hierarchyReport .
(rpt occurrencesOf: Character lf ) < 300 ifTrue:[
   nil error:'report too small'
].
f := GsFile openWriteOnServer:'hierarchyPreMonticello.txt' .
f nextPutAll: rpt .
f close .
true
%

commit
logout

set user DataCurator pass swordfish
login

input $imageRubyDir/GsCompilerClasses.gs

! Arrange to load Metacello/Monticello/OmniBrowser/Release support
run
UserGlobals
  at: #BootstrapConfigurationLoads
  put: #( 'Dev' ).
true
%
commit

set class PositionableStream
list method position
list method position:

input $upgradeDir/installMaster30.topaz

commit
logout

set user SystemUser pass swordfish
login
input $imageRubyDir/pre_prim_methods.gs
run
{ Object . 
   Behavior . 
   UndefinedObject . 
   Boolean
 } do:[:aCls |
   aCls changeToSecurityPolicyForRubyExtension: DataCuratorObjectSecurityPolicy .
].
true
%
commit
logout

set user DataCurator pass swordfish
login

expectvalue true
run
| rpt f |
rpt := ClassOrganizer new hierarchyReport .
(rpt occurrencesOf: Character lf ) < 300 ifTrue:[
   nil error:'report too small'
].
f := GsFile openWriteOnServer:'hierarchyPostMonticello.txt' .
f nextPutAll: rpt .
f close .
true
%
logout
