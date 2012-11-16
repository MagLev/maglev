ruby-support
resolve: path
  "Resolves and returns the object in the Ruby namespace named by path.
   A path is a double-colon separated string like 'Maglev::System'."
  |ns components|
  ns := Object .
  components := path subStrings: '::'.
  (components at: 1) = 'Object' ifTrue:[ components removeAtIndex: 1 ].
  ^ RubyWrapper on: (components inject: Object
               into: [:ns :comp| (ns rubyConstAt: (comp asSymbol) env: 1) value ])

