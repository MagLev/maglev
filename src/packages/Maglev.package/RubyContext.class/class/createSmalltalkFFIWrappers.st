*maglev-runtime
createSmalltalkFFIWrappers
    | year directory classes envId |
    envId := 1 .
    RubyContext load .
    year := DateAndTime now year printString.
    directory := '$MAGLEV_HOME/lib/ruby/site_ruby/1.9/smalltalk/'.
    (GsFile isServerDirectory: directory) == true ifFalse:[ | dirOk |
        dirOk := GsFile createServerDirectory: directory .
        dirOk ifNil:[ Error signal:'failed to create directory' ].
    ].
    classes := Module _wrappedSmalltalkClasses .
    1 to: classes size by: 2 do:[:n | | eachClass stName file isRubyCls inDcSeg |
            eachClass := classes at: n .
            stName := eachClass name .
            isRubyCls := (eachClass nameSpace: envId ) ~~ nil .
            inDcSeg := eachClass objectSecurityPolicy == DataCuratorObjectSecurityPolicy .
            file := GsFile openWriteOnServer: directory , eachClass name , '.rb'.
            [  GsFile gciLogServer: '-- writing: ', file name .
        file nextPutAll:
'#--
# ' , stName, '.rb
#
# Copyright (c) ' , year , ' GemStone Systems, Inc.  All rights reserved.
#
#++

# = Overview
#
# This file defines the methods available in the Smalltalk class ' , stName , '.
# It is not intended for general use in applications, but for use in developing tools
# and persistence libraries to be used by applications.
'       ; cr .

        inDcSeg ifFalse:[
          file nextPutAll: 
'# ' , stName , ' should not be extended in Ruby (it is in SystemObjectSecurityPolicy) ' ; cr; cr.
        ].
        eachClass allSuperClassesDo: [:eachSuperClass|
            file nextPutAll: 'require ''smalltalk/', eachSuperClass name, '''' ; cr
        ] .
        file cr .

        file nextPutAll:'module Smalltalk' ; cr ;
             nextPutAll:'  ', stName , ' = __resolve_smalltalk_global(:' , stName , ')' ; cr .

         isRubyCls  ifTrue:[ | rName |
            file nextPutAll:'end' ; cr ;
                 nextPutAll:'class ' , (rName := eachClass rubyFullName: envId )  ; cr ;
                 nextPutAll:'    # ', rName , ' is identically Smalltalk::', stName ; cr .
        ] ifFalse:[
            file nextPutAll:'  class ' , stName ; cr .
        ].
        eachClass class selectors asSortedCollection do: [:eachSelector |
            | string |
            string := eachSelector copyReplaceChar: $: with: $_  .
            file
               nextPutAll: '    class_primitive_nobridge ''_st_';
               nextPutAll: string;
               nextPutAll: ''', ''';
               nextPutAll: eachSelector;
               nextPutAll: '''';
               cr.
        ].
        eachClass selectors asSortedCollection do: [:eachSelector |
            | string |
            string := eachSelector copyReplaceChar: $: with: $_  .
            file
              nextPutAll: '    primitive_nobridge ''_st_';
              nextPutAll: string;
              nextPutAll: ''', ''';
              nextPutAll: eachSelector;
              nextPutAll: '''';
              cr.
        ].
        isRubyCls ifFalse:[ file nextPutAll:'  end' ; cr ].
        file nextPutAll: 'end'; cr .
    ] ensure: [
        file close.
    ].
].

