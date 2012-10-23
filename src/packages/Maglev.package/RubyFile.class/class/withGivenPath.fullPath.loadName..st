*maglev-runtime
withGivenPath: given fullPath: full loadName: ln
    "Create a Ruby file with the given path information.  An error is
    raised if the full path does not exist."
    (self isFile: full) ifFalse: [ self error: 'file not found: ', full asString ] .
    ^ self new initializeGivenPath: given fullPath: full loadName: ln.

