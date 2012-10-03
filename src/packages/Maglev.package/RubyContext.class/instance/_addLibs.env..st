*maglev-runtime
_addLibs: libString env: envId 
    "Add libraries specified by libString to $:.  The libs may be a single library or
    several libraries separated by ':'."
    | newLib rns |
    newLib := libString findTokens: ':' .
    rns := Object transientNameSpace: envId .
         "GsFile gciLogServer: '-- Adding: ', libString printString, ' to $:' . "
    (rns associationAt: #'$:') globalVarValue insertAll: newLib at: 1 .

