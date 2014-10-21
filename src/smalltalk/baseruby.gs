!=========================================================================
! Copyright (C) VMware, Inc. 2008.  All Rights Reserved.
!
! $Id: baseruby.gs 26114 2011-07-08 16:18:18Z stever $
!
! Name - baseruby.gs
!    top level filein input script for slowrubyimage step of the build
!========================================================================
set user SystemUser pass swordfish
login

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

input $imageDir/fileinImageRubyDir.gs

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
