*maglev-runtime
rubySelectorForFrame: anInteger
	^ (GsProcess _methodInFrameContents: (GsProcess _frameContentsAt: anInteger)) selector

