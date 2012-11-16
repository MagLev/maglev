*maglev-runtime
evalMethodSource: aString with: vcGlobalsArr binding: aBinding
  "given a String and a RubyBinding, build the complete source string   for an eval "
   | methSrc lf |
   lf := Character lf .
  "$~, $_  initialized to default nil"
   (methSrc :=  'begin' copy ) add: lf ;
          add: aString ; add: lf ;
          add: 'ensure'; add: lf ;
          add: '  Thread.__evVcGput($~, $_)' ; add: lf ;
          add: 'end' ; add: lf .
   ^ methSrc

