Used to access a global or constant such as
       a = MA::Y
     has this MRI sexp:
      [:lasgn, :a, [:colon2, [:const, :MA], :Y]]
    The  [:colon2...]  produces a RubyColon2Node
    
    Colon2 nodes also used as name nodes in
      RubyClassNode , RubyModuleNode and RubyConstDeclNode
   