*maglev-runtime
_singletonClassFor: envId
  |isSecondMetaOrSingleton isSecondSingletonGenerated isSecondSingletonAllowed|
  (self _singletonClassAllowedFor: self) ifFalse: [ 
    "singleton class generation not allowed"
    Kernel @ruby1:warn: 'warning: adding higher level singleton class not allowed for ', self name.
    ^ self virtualClass].
  self isBehavior
    ifFalse: ["an object" 
      self virtualClass isRubySingletonClass ifFalse: [self addRubySingletonClass]]
    ifTrue: ["a class"
      (self isMetaOrModule not and: [self isRubySingletonClass not])
        "ifTrue: It's neither a meta class nor a Ruby singleton class, so it must be a class.
         Smalltalk meta classes (=first level Ruby singleton class) are automatically
         created by GemStone."
        ifFalse: [
          self virtualClass isRubySingletonClass
            "ifTrue: singleton class exists already."
            ifFalse: ["The actual singleton class was not created yet. This is just a fix for Trac779."
              self addRubySingletonClass]]].

  "Generate next level singleton class, only if it does not already exist."
  "It's a class (no meta class or singleton class), so meta class exists already."
  isSecondMetaOrSingleton := self virtualClass isMeta not and: [self virtualClass isRubySingletonClass not].
  "Next level singleton class was already generated."
  isSecondSingletonGenerated := self virtualClass virtualClass isRubySingletonClass and: 
    ["check if this singleton class is actually the singleton class for our class or just
      a dummy singleton class for Trac779."
      self virtualClass virtualClass destClass == self virtualClass].
  "Generation of singleton class is allowed for this class. Reimplementing isKindOf: might
   remove this restriction (isKindOf: only works for 'real' classes)."
  isSecondSingletonAllowed := self _singletonClassAllowedFor: self virtualClass.
  (isSecondSingletonGenerated not & isSecondMetaOrSingleton not & isSecondSingletonAllowed)
    ifTrue: [self virtualClass addRubySingletonClassSecondLevel: true].

  ^ self virtualClass

