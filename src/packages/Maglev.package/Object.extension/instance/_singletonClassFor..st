*maglev-runtime
_singletonClassFor: envId
   | cls |
   self isBehavior
     ifFalse: ["an object" 
       self virtualClass isRubySingletonClass ifFalse: [self addRubySingletonClass]]
     ifTrue: ["a class"
       (self isMeta not and: [self isRubySingletonClass not])
         ifTrue: ["It's neither a meta class nor a Ruby singleton class, so it must be a class.
          Smalltalk meta classes (=first level Ruby singleton class) are automatically
          created by GemStone."
           ^ self virtualClass]
         ifFalse: [
           self virtualClass == self _rubySuperclass virtualClass
             ifTrue: ["The actual singleton class was not created yet. This is just
             a fix for Trac779."
               self addRubySingletonClass]
             ifFalse: ["singleton class already exists"
               ^ self virtualClass]]].
  ^ self virtualClass
