class Module

  # Binding::LexicalPath is identically Smalltalk class RubyLexicalPath
  LexicalPath = Object.__resolve_smalltalk_global( :RubyLexicalPath )
end
Module.__freeze_constants

