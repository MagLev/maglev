
module Mod
  autoload("Class", File.expand_path('Trac450_test1.rb', File.dirname(__FILE__)))
end

Mod::Class.superclass

o = Mod::Class.new
unless o.method_2 == 'end of 450' ; raise 'error'; end
puts "Done 450"
true
#################### Trac Info
# ID:         450
# Summary:    Apparent compiler error
# Changetime: 2009-04-21 16:53:33+00:00
###

#  The following suggests ifNil: is not compiling to the correct code as the expression that is the receiver is not nil but the block is executed.
#  
#  topaz 1> frame 86
#  86 RubyCompiler >> defineModuleNamed:rubyMethod:inScope: (envId 0) @8 line 6
#      receiver [105946113  RubyCompiler] aRubyCompiler
#      aSymbol [106780417 sz:7  Symbol] Helpers
#      aNode [105732865  RubyClassBodyNode] aRubyClassBodyNode
#      aParent [98229249  virtualActionView] ActionView
#      aModu [106099713  Module]      Helpers
#  topaz 1> list
#     defineModuleNamed: aSymbol rubyMethod: aNode inScope: aParent 
#       "aParent is a Module or the top  RubyGlobalScopN "
#        | aModu |
#        (aModu :=  aParent classAtOrNil: aSymbol ) ifNil:[  
#               (aModu := Module newModule ) name: aSymbol .
#               aParent at: aSymbol putClass: aModu .     
#   *                   ^8                                               *******
#        ].
#        self extendModule: aModu rubyMethod: aNode .
#        ^ aModu 
#  
#  