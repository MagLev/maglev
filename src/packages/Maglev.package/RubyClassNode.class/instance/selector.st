*maglev-runtime
selector
   ^ superNode ifNil: [ #defineClassNamed:rubyMethod:inScope:fixedIvs: ]
               ifNotNil: [ #defineClassNamed:rubyMethod:inScope:superclass:fixedIvs:  ]

