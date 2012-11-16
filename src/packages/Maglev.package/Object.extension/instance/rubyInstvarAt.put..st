*maglev-runtime
rubyInstvarAt: aString put: aValue 
  "a ruby primitive "
  ^ self rubyInstvarAt: aString put: aValue env: 1"__callerEnvId" 

