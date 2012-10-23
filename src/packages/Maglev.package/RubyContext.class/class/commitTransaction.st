*maglev-runtime
commitTransaction
  System commitTransaction ifFalse:[ | cnfDict arr ex |
	 cnfDict := System transactionConflicts .
	 arr := { } .
	 cnfDict associationsDo:[:assoc | arr add: assoc ].
	 (ex := Error new) messageText: 'commit failed' ; args: arr .
	 ex signal .
	 ^ false 
  ].
  ^ true

