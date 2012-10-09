ruby-support
runRuby: file
  "
   This is the entry point for running ruby.  The Compiler manages a stack
   of files, so we wrap our source code into a RubyEvalFile object, which
   is the Compiler's API for getting source code to work on.

   This method emulates RubyFile>>loadIntoEnv: We assume we are not loading
   a c-extension.
  "
  | oldFile fileStk |

  oldFile := tns at: #'__FILE__' otherwise: nil .
  tns at: #'__FILE__' compilePut: (file fullPath).
  fileStk := (RubyCompilerState initialize: envId) fileStack .
  fileStk push: file .
  ^ [ self compileFile: file ]
    ensure: [ fileStk pop: file .
              tns  at: #'__FILE__'  compilePut: oldFile. ]

