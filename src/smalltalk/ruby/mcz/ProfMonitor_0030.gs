
set class ProfMonitor
category: '*maglev-runtime'
classmethod:
newIntervalNs: nsPerSample
	"initialize an instance with sampling suspended"
	| inst result |
	inst := self basicNew initializeNoFile .
	inst _createFile: self newProfileFileName;  intervalNs: nsPerSample .
	inst startMonitoring .
	inst suspendSampling .
	^ inst

%


set class ProfMonitor
category: '*maglev-runtime'
method:
stopMonitoringReport
	"Stop monitoring and produce a report."
	self stopMonitoring .
	^ self reportAfterRun .

%

