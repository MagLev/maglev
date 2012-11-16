as yet unclassified
walkRpList: list 
  "used by ParAsgnRpNode>>walkRpNode"
  1 to: list size do:[:n |
	(list at: n) walkRpNode
  ].

