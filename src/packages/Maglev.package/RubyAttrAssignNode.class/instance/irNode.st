*maglev-runtime
irNode
  | irArgs na fullSel snd tasgn tmpLeaf |
  irArgs := self irArgNodes .
  na := irArgs size  .
  fullSel := self fullSelector .
  tasgn := GsComAssignmentNode _basicNew dest:( tmpLeaf := resTmp leaf )
                   source: (irArgs at: na)   .
  self ir: tasgn .
  irArgs at: na put: tasgn .
  (snd := GsComSendNode new)  rcvr:  receiverNode irNode ;
       rubySelector: fullSel .
  self buildArgumentsOn: snd .
  self ir: snd . 
  ^ GsComStatementsNode new list: { snd . (GsComVariableNode new leaf: tmpLeaf) }.

