as yet unclassified
sourceForIR: irNode
	|strm|
	strm := IndentingStream newPrinting.
	irNode printFormattedOn: strm.
	^ strm contents.
	

