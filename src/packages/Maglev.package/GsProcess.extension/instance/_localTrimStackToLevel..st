*maglev-override-Debugging Support
_localTrimStackToLevel: aLevel

"Deletes stack entries from 1 to (aLevel - 1) inclusive, thus making aLevel the
 new top-of-stack(TOS).  At new TOS, looks in the implementation class of the
 method at TOS, using the selector of the method at TOS.  If that class's method
 dictionary contains a different GsNMethod, the GsNMethod currently in the method
 dictionary is installed as the method at the TOS.  The saved instruction
 pointer for TOS is set to the entry of the method at TOS.

 Limitations:
   If the receiver was executing in native code, and the stack
   is trimmed, any resumption of execution will be in interpreted mode.

   If the new top-of-stack is an anonymous method, it is not possible to
   determine whether that method has been recompiled, and the method at new top
   of stack will not be changed.  Debuggers should use the
   GsNMethod | _recompileWithSource method to implement recompilation from a
   debugger pane.  _recompileWithSource: raises an error on attempts to
   recompile an anonymous method.

   Raises an error if the new top-of-stack would represent the entry to an
   ExecBlock and the home method of the block has been recompiled.

   Raises an error if the new top-of-stack would represent a return 
   across an FFI , Ruby C extension, or UserAction call.

 Has no effect if aLevel is out of range or aLevel < 2.

 Debuggers must not cache or directly manipulate VariableContexts 
 when examining or altering stacks.

 Provides the ability for a debugger to restart execution after recompiling
 a method that is active on the stack of the receiver."

 | fpIdx aMeth oldHome oldmClass newTosIdx oldDepth newDepth newTosFpOfs 
   prevFpIdx envId |
  self _nativeStack ifTrue:[
    self _convertToPortableStack .
  ].
  "check for argument aLevel out of range"
  oldDepth := self localStackDepth .
  (aLevel < 2 or:[ aLevel > oldDepth ]) ifTrue:[ ^ self ].
  newDepth := oldDepth - (aLevel - 1) .

  1 to: aLevel do:[:n| 
    prevFpIdx := fpIdx .
    fpIdx := self _localFrameOffsetAt: n  . 
    aMeth := self _fetchMethod:( arStack at:( fpIdx + FP_codePtr_OFS)) .
    aMeth ifNil:[ self _halt:'Cannot trim stack across a reenter marker'].
  ].
  
  "check to see whether new TOS method has been recompiled, and if so, 
   install the new method."
  oldHome := aMeth homeMethod .
  oldmClass := oldHome inClass .
  envId := (oldHome respondsTo: #environmentId) ifTrue: [oldHome environmentId] ifFalse: [0].
  (oldmClass isNil or: [self class anonymousSelectors includes: oldHome selector]) ifFalse: [
    "not an anonymous method"
    | newMeth oldCptr |
    newMeth := oldmClass compiledMethodAt: oldHome selector environmentId: envId.
    newMeth == aMeth ifFalse:[
      aMeth == oldHome ifFalse:[
        self _halt:'Cannot trim stack to a block in recompiled method'.
      ].
      "install recompiled method"
      oldCptr := arStack at: (fpIdx + FP_codePtr_OFS) . 
      oldCptr class == GsNMethod ifTrue:[
        arStack at: (fpIdx + FP_codePtr_OFS) put: newMeth .
      ] ifFalse:[
        self _halt:'should not be here'
      ].
    ].
  ].
  newTosIdx := fpIdx + FP_codePtr_OFS .
  "push savedIP which is byte offset to first instr in the method"
  newTosIdx := newTosIdx - 1 .
  arStack at: newTosIdx put: OC_GSNMETHOD_FIRST_INSTR_OFFSET .
  "don't need  _tosIsIpNil in arStack object"

  newTosFpOfs := arStack at: prevFpIdx .
  topFpOffset := newTosFpOfs .  "adjust offset to Top FP"

  "delete the unwanted stack frames.  
     Since FP linkages in arStack are relative to   arStack size    
     there is no other fixup needed. "
  arStack removeFrom:1 to: newTosIdx - 1 .

  "adjust the depth of the receiver."
  uaCount_depth := ((uaCount_depth bitShift: -32)  bitShift:32)  
         bitOr: (newDepth bitAnd: 16rFFFFFFFF) .
  
  dbgFpsCache := nil . "clear frame offsets cache"

