
set class TransientMutex class
category: '*maglev-runtime'
method:
comment
  ^ 'TransientMutex instances are DbTransient'

%


set class TransientMutex class
category: '*maglev-runtime'
method:
forRubyMutualExclusion
  ^ self rubyBasicNew

%


set class TransientMutex class
category: '*maglev-runtime'
method:
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

