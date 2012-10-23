*maglev-runtime
withFile: rubyFile do: aBlock
	"Run the block with rubyFile as the current file."
	^ rubyFile  exists ifTrue: [aBlock value: rubyFile ].
    

