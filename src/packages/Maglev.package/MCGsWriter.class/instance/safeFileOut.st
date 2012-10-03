as yet unclassified
safeFileOut
	| maglevHome path |
	(maglevHome := (RubyEnv _getenv: 'MAGLEV_HOME')) ifNil: [ ^ self ].
	path := maglevHome, '/src/smalltalk/ruby/mcz'.
	(FileDirectory on: path) exists ifFalse: [ ^ self ].
	self fileOutIn: path.

