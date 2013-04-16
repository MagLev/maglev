# file syntaxErrorsTest.rb

# Maglev.__system.session_temp_put( :DebugParse , false)
count = 0
arr = [
 'syntaxErrDynamicConstant.rb',
 'syntaxErrFormalArgConstant.rb',
 'syntaxErrConstantReassign.rb',
 'syntaxErrFormalArgIv.rb',
 'syntaxErrFormalArgCv.rb',
 'syntaxErrOddSizeHash.rb',
 'syntaxErrModuleInMeth.rb',
 'syntaxErrBeginInMeth.rb',
 'syntaxErrEndInMeth.rb',
 'syntaxErrEmptySym.rb',
 'syntaxErrTooManyEnd.rb' ,
 'syntaxErrTooFewEnd.rb' ,
 'syntaxErrLParen1.rb',
 'syntaxErrLParen2rb',
 'syntaxErrRParen1.rb',
 'syntaxErrRParen2.rb',
 'syntaxErrLBrack1.rb',
 'syntaxErrLBrack2.rb',
 'syntaxErrRBrack2.rb',
 'syntaxErrRBrack3.rb',
 'syntaxErrOpenQuote.rb',
 'syntaxErrOpenDQuote.rb',
 'syntaxErrSymWithNull.rb',
 'syntaxErrAliasNthRef.rb',
 'syntaxErrClassNameNotConst.rb'
]

arr.each do |fn|
  got_ex = false
  begin
    load "src/test/#{fn}"
  rescue SyntaxError => ex
    got_ex = true
    count += 1
  end
  unless got_ex
    raise 'failed to get exception'
  end
#  Maglev.__system.session_temp_put( :TrapParse, false )
end

unless count == arr.size ; raise 'w rong number of exceptions'; end
true
