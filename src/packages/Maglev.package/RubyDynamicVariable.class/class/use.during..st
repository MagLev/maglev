*maglev-runtime
use: anObject during: aBlock
    ^ aBlock
        onException: self
        do: [:n | n resume: anObject]

