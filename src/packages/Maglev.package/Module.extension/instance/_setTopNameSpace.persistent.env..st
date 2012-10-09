*maglev-runtime
_setTopNameSpace: tns persistent: pns  env: envId
  | ofs |
  "Private initialize persistent name space for Object , for bootstrap only."
  self == Object ifFalse:[ self error:'can only be applied to Object' ].
  self initNameSpacesForExtend: envId .
  ofs := envId*4 + 2  .
  methDicts _rubyAt: ofs - 1 put: pns .
  transientMethDicts _rubyAt: ofs - 1 put: tns

