
set class TransientMutex
category: '*maglev-runtime'
classmethod:
comment
  ^ 'TransientMutex instances are DbTransient'

%


set class TransientMutex
category: '*maglev-runtime'
classmethod:
forRubyMutualExclusion
  ^ self rubyBasicNew

%


set class TransientMutex
category: '*maglev-runtime'
classmethod:
rubyBasicNew

^ self rubyBasicNew_stBaseClass: TransientMutex

%


set class TransientMutex
category: '*maglev-runtime'
method:
isLocked
   ^ self semaphore isLocked

%


set class TransientMutex
category: '*maglev-runtime'
method:
tryLock 
   ^ self semaphore tryLock

%

