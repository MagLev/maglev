*maglev-runtime
newIntervalNs: nsPerSample
	"initialize an instance with sampling suspended"
	| inst result |
	inst := self basicNew initializeNoFile .
	inst _createFile: self newProfileFileName;  intervalNs: nsPerSample .
	inst startMonitoring .
	inst suspendSampling .
	^ inst

