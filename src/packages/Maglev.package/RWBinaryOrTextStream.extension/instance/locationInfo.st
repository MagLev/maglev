*maglev
locationInfo
 
  ^ { position . 
       itsCollection copyFrom: position to: (position + 40 min: itsCollection size)  . 
       itsCollection copyFrom: ( position - 200 max: 1 )
                     to: (position + 200 min: itsCollection size) . 
       itsCollection
     }.

