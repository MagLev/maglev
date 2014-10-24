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

