*maglev-runtime
loadPath: envId
  "The result may be an empty array"
  ^ ((Object transientNameSpace: envId) resolveConstant: #'$:') globalVarValue .

