as yet unclassified
topLineBiasOrZero
  | res |
  res := lineBiasStack topOrNil .
  ^ res ifNil:[ 0 ] 

