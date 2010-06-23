# Maglev generates a stack overflow with this
#   ==> 1 Exception >> _pass:with:                 (envId 0) @8 line 23
#   2 Exception >> outer                       (envId 0) @2 line 19
#   3 [] in  RubyCompiler >> compileFileNamed:loadName:env: (envId 0) @6 line 43
#   4 Exception >> _executeHandler:            (envId 0) @3 line 8
#   5 Exception >> _pass:with:                 (envId 0) @4 line 18
#   6 Exception >> outer                       (envId 0) @2 line 19
#   7 [] in  RubyCompiler class >> withRubyHandlers:main:do: (envId 0) @17 line 46
#   8 Exception >> _executeHandler:            (envId 0) @3 line 8
#   9 Array # __joinStrings                    (envId 1) @1 line 1
#   10 Kernel # method_missing:*                (envId 1) @9 line 11
#   11 Object >> _doesNotUnderstand:args:envId:reason: (envId 0) @35 line 31
#   12 Array # __joinStrings                    (envId 1) @9 line 7
#   13 Kernel # method_missing:*                (envId 1) @9 line 11
#   14 Object >> _doesNotUnderstand:args:envId:reason: (envId 0) @35 line 31
#   15 Array # __joinStrings                    (envId 1) @9 line 7
#   16 Kernel # method_missing:*                (envId 1) @9 line 11
#   17 Object >> _doesNotUnderstand:args:envId:reason: (envId 0) @35 line 31
#   18 Array # __joinStrings                    (envId 1) @9 line 7
#   19 Kernel # method_missing:*                (envId 1) @9 line 11
#   20 Object >> _doesNotUnderstand:args:envId:reason: (envId 0) @35 line 31
#   21 Array # __joinStrings                    (envId 1) @9 line 7
#   22 Kernel # method_missing:*                (envId 1) @9 line 11
#   ...

# MRI says Trac752.rb:34: protected method `to_s' called for foo:C (NoMethodError)

class C
  protected
  def to_s
    "foo"
  end
end

puts C.new.to_s
true
