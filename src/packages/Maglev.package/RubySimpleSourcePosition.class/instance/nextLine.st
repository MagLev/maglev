as yet unclassified
nextLine
  "Used for ad-hoc adjustments for MRI parser position defects."
  | res | 
  (res := self class _basicNew) 
     filename: filename ;
     line: line + 1 .
  ^ res

