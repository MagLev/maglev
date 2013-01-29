!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================
!

! Remove existing behavior from RubyIdentityHash
doit
RubyIdentityHash removeAllMethods.
RubyIdentityHash class removeAllMethods.
true
%
category: 'Accessing'
set compile_env: 0
method: RubyIdentityHash
isIdentityHash
   ^ true
%
category: 'Updating'
set compile_env: 0
method: RubyIdentityHash
isIdentityHash: newValue
  "Identity hash cannot be converted back to normal hash."
%
run
RubyIdentityHash category: 'Ruby support'.
true
%
