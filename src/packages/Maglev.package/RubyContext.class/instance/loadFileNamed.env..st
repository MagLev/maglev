*maglev-runtime
loadFileNamed: aString env: envId
    | ary file |
    ary := RubyFile findRubyFileFor: aString isRequire: false env: envId .
    file := ary at: 1 . 
    (file == nil or:[ file exists not]) ifTrue:[
        RubyLoadError signal: 'no such file to load -- ', aString 
    ] .
    ^ file loadIntoEnv: envId . 

