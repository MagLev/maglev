!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: NilTF.gs 25988 2011-06-14 17:55:54Z stever $
!
!  This file contains additional Smalltalk methods for
!   UndefinedObject and Boolean   to support the 
!  Ruby classes  NilClass, TrueClass, FalseClass
!
!=========================================================================

category: 'Ruby support'

! ----------------------------------------
set class Boolean
classmethod:
useRubyClasses: aBoolean  bootStrap: forBootBoolean
  "If aBoolean==true, adjust the VM's class lookup table for 
   special objects so that   
     true class == TrueClass
     false class == FalseClass
   else adjust the table so that
     true class == Boolean
     false class == Boolean .
   The class lookup table also controls the class used for method 
   lookup when receiver is true or false .
   If aBoolean == false, also initializes the Ruby C extensions implementation.
   Returns receiver.  
   Invoked during Ruby Session initialization. "
<primitive: 849>
aBoolean _validateClass: Boolean .
forBootBoolean _validateClass: Boolean
%

classmethod:
usingRubyClasses
  ^ true class == TrueClass 
%

! ----------------------------------------
set class UndefinedObject

classmethod:
noArgNil
  ^  _rubyNoArgNil
%

method:
nthRegexRef: anInt
 "to support access to $0..$9 when $~ is nil"
^ nil
%
! end UndefinedObject

! pre_match, post_match, plus_match  all in .rb files now
