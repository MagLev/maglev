! for interactive filein of the Smalltalk code with topaz
! requires these environment variables set 
!   export imageDir=$MAGLEV_HOME/src/smalltalk
!   export imageRubyDir=$MAGLEV_HOME/src/smalltalk/ruby
display pauseone
omit pushonly
output push filein.out
inp $imageDir/baseruby.gs

inp $imageDir/loadfiletree.gs
output pop
