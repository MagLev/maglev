
set class RubyProcessStatus
category: '*maglev-runtime'
classmethod:
with: rawResultArray
  ^ self _basicNew init: rawResultArray

%


set class RubyProcessStatus
category: '*maglev-runtime'
method:
init: rawResultArray
  stat := rawResultArray at: 1 . "rawStatus from the child"
  primStatus := rawResultArray .

%


set class RubyProcessStatus
category: 'as yet unclassified'
method:
status
 ^ stat

%

