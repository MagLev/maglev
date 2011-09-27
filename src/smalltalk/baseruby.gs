!=========================================================================
! Copyright (C) VMware, Inc. 2008.  All Rights Reserved.
!
! $Id: baseruby.gs 26114 2011-07-08 16:18:18Z stever $
!
! Name - baseruby.gs
!    top level filein input script for slowrubyimage step of the build
!========================================================================
set user SystemUser pass swordfish
! iferr 1 exit
login

iferr 1 where  
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

input $upgradeDir/ruby/GsCompilerClasses.gs

input $upgradeDir/ruby/Behavior_ruby.gs

! class definitions
input $upgradeDir/ruby/Classes_ruby.gs

input $upgradeDir/ruby/Class_ruby.gs
input $upgradeDir/ruby/Metaclass3_ruby.gs
input $upgradeDir/ruby/Capi_ruby.gs
input $upgradeDir/ruby/ObsoleteMetaclass_ruby.gs
input $upgradeDir/ruby/UnorderedCollection_ruby.gs

! methods for GsCompilerIRNode classes
input $upgradeDir/ruby/GsCompilerIRNode.gs
input $upgradeDir/ruby/GsNMethod_ruby.gs

! Smalltalk env 0 and env1 methods for other base classes
input $upgradeDir/ruby/Association_ruby.gs
input $upgradeDir/ruby/Array_ruby.gs
input $upgradeDir/ruby/CharacterCollection_ruby.gs
input $upgradeDir/ruby/ExecBlock_ruby.gs
input $upgradeDir/ruby/Exception_ruby.gs
input $upgradeDir/ruby/IO_ruby.gs
input $upgradeDir/ruby/GsFile_ruby.gs
input $upgradeDir/ruby/GsProcess_ruby.gs
input $upgradeDir/ruby/GsSocket_ruby.gs
input $upgradeDir/ruby/Numeric.gs
input $upgradeDir/ruby/MatchData.gs
input $upgradeDir/ruby/NilTF.gs
input $upgradeDir/ruby/Object_ruby.gs
input $upgradeDir/ruby/ProcessorScheduler_ruby.gs
input $upgradeDir/ruby/Repository_ruby.gs
input $upgradeDir/ruby/Symbol_ruby.gs
input $upgradeDir/ruby/SymbolAssociation_ruby.gs
input $upgradeDir/ruby/String_ruby.gs
input $upgradeDir/ruby/System_ruby.gs
input $upgradeDir/ruby/Range_ruby.gs
input $upgradeDir/ruby/Regexp.gs
input $upgradeDir/ruby/RubyConstantRef.gs
input $upgradeDir/ruby/RubyDirectory.gs
input $upgradeDir/ruby/RubyEnv.gs
input $upgradeDir/ruby/RubyProc.gs
input $upgradeDir/ruby/RubyThreadGroup.gs
input $upgradeDir/ruby/RubySocket.gs 
input $upgradeDir/ruby/IPSocket.gs
input $upgradeDir/ruby/TCPSocket.gs
input $upgradeDir/ruby/TCPServer.gs
input $upgradeDir/ruby/UDPSocket.gs
input $upgradeDir/ruby/UNIXSocket.gs
input $upgradeDir/ruby/UNIXServer.gs
input $upgradeDir/ruby/TransientShortArray_ruby.gs
input $upgradeDir/ruby/RubyHash.gs
input $upgradeDir/ruby/RubyIdentityHash.gs
! DateTime_ruby.gs no longer used
input $upgradeDir/ruby/VariableContext_ruby.gs
input $upgradeDir/ruby/Module_ruby.gs
input $upgradeDir/ruby/Kernel.gs
input $upgradeDir/ruby/RubySignal.gs
input $upgradeDir/ruby/RubyTime.gs
input $upgradeDir/ruby/ProfMonitorTree_ruby.gs
input $upgradeDir/ruby/ProfMonitorEntry_ruby.gs
input $upgradeDir/ruby/GsObjectSecurityPolicies.gs

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

input $upgradeDir/ruby/GsCompilerClasses.gs

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
input $upgradeDir/ruby/pre_prim_methods.gs
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
