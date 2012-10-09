accessing
_setGlobalsFrom: globals env: envId
	"For each of the key value pairs, set the corresponding transient global"
	| tns |
	tns := Object transientNameSpaceForStore: envId .
	globals keysAndValuesDo: [ :key :value | tns rubyGlobalVar: key put: value ] .
	^ self .

