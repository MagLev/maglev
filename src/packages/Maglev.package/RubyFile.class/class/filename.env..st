*maglev-runtime
filename: aString env: envId
"Create and return a file named aString.  Will try to find the file using the current ruby path $:.
If the file is not found, returns nil.  Does not try to add .rb or any other suffix (see RubyContext>>requireFileNamed)."
|file|
file := self new setFilename: aString env: envId .
file path == nil ifTrue:[    ^ nil ] . "file not found"
^ file .

