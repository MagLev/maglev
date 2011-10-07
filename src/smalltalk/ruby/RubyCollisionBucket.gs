!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: $
!
!=========================================================================

+++ obsolete file, no longer used

set class RubyCollisionBucket

removeallmethods
removeallclassmethods

category: 'Ruby support'

method:
compareKey: key1 with: key2

"Collision buckets need to respect ruby semantics and use eql? rather than =="
"(envId := self __threadRubyEnvId) == 1 ifTrue:["
  ^ key1 @ruby1:eql?: key2
"] ifFalse:[
  ^ key1 with: key2 perform: #'eql?:' env: envId
 ]"
%

