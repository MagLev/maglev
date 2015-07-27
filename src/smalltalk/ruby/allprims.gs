set user DataCurator pass swordfish
login
run
| found |
(found := RubyContext reset) ifTrue:[
  UserGlobals removeKey:#RubyPrimsLoaded ifAbsent:[].
  RubyContext commitTransaction
].
^ found
%
logout
set user SystemUser pass swordfish
login
run
"ProtoObject is loaded into DataCurator's UserGlobals by the
  loadfiletree step,  add a reference from SystemUsers' UserGlobals"
UserGlobals at:#ProtoObject put:
((AllUsers userWithId:'DataCurator') symbolList resolveSymbol:#ProtoObject)
  value
%

input $MAGLEV_HOME/src/smalltalk/ruby/pre_prim_methods.gs
run
{ Object . 
   Behavior . 
   ProtoObject .
   UndefinedObject . 
   Boolean 
 } do:[:aCls |
   aCls changeToSecurityPolicyForRubyExtension: DataCuratorSegment .
].
true
%
commit
logout
set user DataCurator pass swordfish
login
run
  "For issue 269, make Object's env 1 superclass ProtoObject before
   Object includes Kernel"
  Object persistentRubySuperclass: 1 put: ProtoObject .
  true
%
commit
run
 RubyBridge initialize . "reset generic bridge methods"
 RubyContext commitTransaction  .
%
run
RubyContext load: #( ) env: 1 
%
run
UserGlobals at:#RubyPrimsLoaded put: true .
RubyContext commitTransaction
%
! caller responsible for topaz exit 

