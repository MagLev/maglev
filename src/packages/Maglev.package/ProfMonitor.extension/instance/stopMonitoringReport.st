*maglev-runtime
stopMonitoringReport
	"Stop monitoring and produce a report."
	self stopMonitoring .
	^ self reportAfterRun .

