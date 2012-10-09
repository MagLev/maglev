patches
loadSource
|file|
  file := GsFile openReadOnServer: path.
  file == nil ifTrue:[
    RubyLoadError signal: 'file not found , ' , path .
  ].
  source := file contents.
  file close

