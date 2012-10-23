as yet unclassified
basicLog: aString
	| f |
	self error:'should not be here'. 
	f := GsFile openAppendOnServer: '/opt/gemstone/log/ruby.log'.
	f cr; nextPutAll: aString.
	f close

