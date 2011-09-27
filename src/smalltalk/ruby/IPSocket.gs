!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: IPSocket.gs 26146 2011-07-11 19:31:45Z otisa $
!=========================================================================

set class IPSocket

removeallmethods
removeallclassmethods

category: 'Ruby support'

method:
rubyAddress
| portNum hostName ipAddrStr prefix |

ipAddrStr := self address .
prefix := '::ffff:' .
(ipAddrStr at: 1 equals: prefix ) ifTrue:[ 
  "translate mapped IP4 to IP4 format"
  ipAddrStr :=  ipAddrStr copyFrom: prefix size + 1 to: ipAddrStr size
].
hostName := self class getHostNameByAddress: ipAddrStr .
portNum := self port .

^ { 'AF_INET' . portNum . hostName . ipAddrStr }
%

method:
rubyPeerAddress

| portNum hostName ipAddrStr |

ipAddrStr := self peerAddress .
hostName := self class getHostNameByAddress: ipAddrStr .
portNum := self peerPort .

^ { 'AF_INET' . portNum . hostName . ipAddrStr }
%
