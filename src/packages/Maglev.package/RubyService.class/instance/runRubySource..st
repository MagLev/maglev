ruby-support
runRubySource: source
  "Creates a fake file and then calls the RubyCompiler on it"
  |fakeFile|
  fakeFile := RubyEvalFile new.
  fakeFile fileName: 'Smalltalk Access to Ruby' .
  fakeFile source: source.
  ^ self runRuby: fakeFile

