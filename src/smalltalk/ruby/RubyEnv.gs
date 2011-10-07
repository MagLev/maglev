!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================
set class RubyEnv
removeallmethods
removeallclassmethods

category: 'Ruby support'

! _currentAssoc implemented in .mcz

classmethod: RubyEnv
_envPrim: opcode with: aString  with: argString
"opcode 0  getAllEnvVars()
        1  getenv(aString)
        2  putenv(aString, aString)
        3  unsetenv(aString)"
<primitive: 766>
aString _validateClass: String  .
opcode >= 1 ifTrue:[
  aString ifNotNil:[ aString _validateClass: String  ].
  opcode == 2 ifTrue:[ argString _validateClass: String  ].
].
self _primitiveFailed:#_envPrim:with:with: 
     args: { opcode . aString . argString }
%

classmethod
_getAllEnvVars

"Returns an Array of environment names and values.
   obtained by enumerating the list
       extern char** environ;   
 which is defined in /usr/include/unistd.h  .
 Each element of the result is a String 
  (result at:1) is the name of the first environment variable,
  (result at:2) is the value of the first environment variable"
 
^ self _envPrim:0 with: nil with: nil
%

classmethod:
_getenv: aString
  " Returns a String, or nil"
  ^ self _envPrim:1 with: aString with: nil
%

classmethod:
_putenv: aKey with: aValue
  "aKey and aValue must be Strings.
   Returns 0 if successful, otherwise an errno SmallInt."

  ^ self _envPrim:2 with: aKey with: aValue
%

classmethod: RubyEnv
_unsetenv: aKey
  "aKey be a String.
   Returns 0 if successful, otherwise an errno SmallInt."

  ^ self _envPrim:3 with: aKey with: nil
%

method:
at: aKey put: aValue
  "invoked from Smalltalk only"

  self class _putenv: aKey with: aValue .
  ^ super @ruby1:__atkey_put: aKey _: aValue 
%
