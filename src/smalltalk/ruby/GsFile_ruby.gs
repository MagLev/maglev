!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: GsFile_ruby.gs 26114 2011-07-08 16:18:18Z stever $
!
!  additional methods  for GsFile to support Ruby 
!=========================================================================

set class GsFile

! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

classmethod:
_basicNew
  ^ self rubyNewCFinalizer_stBaseClass: GsFile
%

classmethod:
_stdinServer

^ self _getStdFile: 0 onClient: false
%
classmethod:
_stdoutServer

^ self _getStdFile: 1 onClient: false
%
classmethod:
_stderrServer

^ self _getStdFile: 2 onClient: false
%

classmethod:
_rubyFopen: aPath mode: aMode
  | inst |
  inst := self _basicNew .
  ^ inst _fopen: aPath mode: aMode 
%
classmethod:
_rubyOpen: aPath mode: flagsInt permission: modeInt
  | inst |
  inst := self _basicNew .
  ^ inst _open: aPath flags: flagsInt mode: modeInt
%

method:
_fopen: aPath mode: aMode
"Uses section 2 open() or  section 3 fdopen() depending on class of aPath.
 Returns receiver or a SmallInt errno "

<primitive: 811>
self _primitiveFailed: #_fopen:mode: args: { aPath . aMode }
%

method:
_open: aPath flags: flagsInt mode: modeInt
"Uses section 2 open() .
 Returns receiver in opened state with isClient == false,  
 or returns a SmallInt errno."

<primitive: 810>
self _primitiveFailed: #_open:flags:mode: args: { aPath . flagsInt . modeInt }
%

classmethod:
fstat: aFileDescriptor isLstat: aBoolean
 
"operates on server files only"
<primitive: 758>
aBoolean _validateClass: Boolean .
aFileDescriptor _validateClass: SmallInteger .
(aFileDescriptor < 1) ifTrue:[ self _errorIndexOutOfRange: aFileDescriptor ].
self _primitiveFailed: #fstat:isLstat: args: { aFileDescriptor . aBoolean }
%


method:
lastRubyErrorString

| errStr |
errStr := self lastErrorString .
^ errStr ~~ nil ifTrue:[ errStr ] ifFalse:[ 'no details' ].
%


classmethod: 
_modifyFile: opcode fdPath: fdOrString with: argOne with: argTwoArray

" Installed in env1 method dicts for ruby selector  _modify_file:::*  .
  opcode 
        0  delete(path)
	1  chmod(path, mode)
        2  truncate(path, length)
        3  chown(path,  owner, group)
        4  lchown(path, owner, group)
        5  utime(path, acctime, modtime)
        6  symlink(path1, path2)
        7  rename(path1, path2)
        8  link(path1, path2)
        9  readlink(path, destString)
       10  fchmod(fd, mode)
       11  flock(fd, operation)
       12  fchown(fd,  owner, group)
       13  fetch_flock_constants(fdIgnored, destArray)
 result of above is a SmallInt, 0 for success, or the C errno 
       14  dirname(path)
       15  ftruncate(fd, length)
       16  fsync(fd)
 result is 0 if successful or an errno value 
 "
<primitive: 763>
opcode _validateClass: SmallInteger .
fdOrString _validateClasses: { SmallInteger . String }.
argOne ifNotNil:[ argOne _validateClasses: { SmallInteger . String }].
argTwoArray ifNotNil:[ | argTwo |
  argTwo := argTwoArray at: 1 .
  argTwo ifNotNil:[ argTwo _validateClasses: { SmallInteger . String }].
].
self _primitiveFailed: #_modifyFile:fdPath:with:with:
     args: { opcode . fdOrString . argOne . argTwoArray }
%

classmethod:
_umask: newMaskInt
" anInt == -1 means return current umask without changing it .
  otherwise set process' file creation mask to newMaskInt
  and return previous value.
"
^ RubyDirectory _prim:5 with: nil with:newMaskInt
%

classmethod:
_section2OpenConstants
  
  "for use during bootstrap .
   RUBY_APPEND , etc, are class variables created by bom.c"
 | h |
 h := RubyIdentityHash new .
 h at: #RUBY_APPEND put: RUBY_APPEND ;
   at: #RUBY_CREAT put: RUBY_CREAT ;
   at: #RUBY_EXCL  put: RUBY_EXCL ;
   at: #RUBY_NOCTTY  put: RUBY_NOCTTY ;
   at: #RUBY_NONBLOCK  put: RUBY_NONBLOCK  ;
   at: #RUBY_RDONLY  put: RUBY_RDONLY ;
   at: #RUBY_RDWR  put: RUBY_RDWR ;
   at: #RUBY_SYNC  put: RUBY_SYNC ;
   at: #RUBY_TRUNC  put: RUBY_TRUNC ;
   at: #RUBY_WRONLY put: RUBY_WRONLY .
 ^ h
%

! chown  fd or path
! lchown       path 
! chmod  fd or path

method:
_popen: commandStr mode: modeChar

"Returns the receiver or a SmallInteger errno.
  Calls the section 3 posix  popen() call "

<primitive: 596>
commandStr _validateClass: String .
modeChar _validateClass: Character .
(modeChar = $w or:[ modeChar = $r ]) ifFalse:[
  ArgumentError signal:'invalid mode to popen'
].
self _primitiveFailed: #_popen:mode: args: { commandStr . modeChar }
%
method: 
_pclosePrimStatus
"Returns nil or a SmallInteger errno, from the 
  underlying pclose() performed on a file opened with _popen:mode:
 Returns nil if the file is not yet closed, or is a client file."

<primitive: 573>
self _primitiveFailed: #_pclosePrimStatus
%

classmethod:
_popen: commandStr mode: modeStr

"Returns a GsFile, or a SmallInteger errno.
  Calls the section 3 posix  popen() call "
| sz |
((sz := modeStr size) == 1 or:[
   (sz == 2 and:[ (modeStr at: 2) == $+ ])]) ifFalse:[
  ArgumentError signal:'invalid mode to popen'
].
^ self _basicNew _popen: commandStr mode: (modeStr at: 1)
%

method:
_byteOp: arg
  " if arg == true ,  implements peekByte,
      and returns -2 if EINTR occurs, nil if error or EOF, else
        SmallInteger 0..255 and returned byte has not been consumed. 
    else if arg == false,  implements getByte,
      and returns -2 if EINTR occurs, nil if error or EOF, else
      returns a consumed byte,  SmallInteger 0..255.  
    else if arg is SmallInteger 0..255
      implments ungetByte, and returns the argument "
<primitive: 856>
arg _validateClasses: { Boolean . SmallInteger } .
"probably a client file instead of a server file"
self _primitiveFailed: #_byteOp: args: { arg }
%

method:
_disableAutoClose

"Disable the automatic close of the receiver's file descriptor that would 
 otherwise happen when the receiver is garbage collected or when the session
 is shutdown"

<primitive: 891>
self _primitiveFailed: #_disableAutoClose
%

method:
peekByte
  | res |
[
  res := self _byteOp: true .
  res == -2  "loop to handle EINTR, so ctl-C works"
] untilFalse .
^ res  "nil indicating EOF or error, or a SmallInteger byte value"
%

method:
nextByte
  "Reimplemented in Ruby dbf to assume server file and use _getByte:"
  | res |
[ 
  res := self _byteOp: false .
  res == -2  "loop to handle EINTR, so ctl-C works"
] untilFalse .
^ res  "nil indicating EOF or error, or a SmallInteger byte value"
%
method
ungetByte: aByteValue

  self _byteOp: aByteValue .
  ^ nil "per Ruby specs"
% 

method:
_isWritable
  
  "Returns true if receiver is open for write/append, otherwise returns false."
  | m |
  (m := mode) ifNil:[ ^ false].
  ^ (m includesValue: $w) or:[ m includesValue: $a ] 
%

