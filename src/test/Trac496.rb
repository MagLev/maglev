require 'pathname'

# File.open was having problems with non-string objects being passed in.
p = Pathname(__FILE__)

lines = File.open(p).readlines
raise 'Fail' unless lines.size > 0

#################### Trac Info
# ID:         496
# Summary:    Methods in Smalltalk classes need to be more adaptable to Ruby arguments
# Changetime: 2009-04-28 00:27:14+00:00
###

#  ERROR 2094, The object aPathname was not of the expected class String.The object aPathname was not of the expected class String.
#  topaz 1> where
#  ==> 1 Exception >> _pass:with:                 (envId 0) @8 line 23
#  2 Exception >> pass                        (envId 0) @2 line 14
#  3 [] in  RubyFile >> load                  (envId 0) @3 line 19
#  4 Exception >> _executeHandler:            (envId 0) @3 line 8
#  5 Exception >> _pass:with:                 (envId 0) @4 line 18
#  6 Exception >> pass                        (envId 0) @2 line 14
#  7 [] in  RubyCompiler >> compileFileNamed:loadName: (envId 0) @7 line 46
#  8 Exception >> _executeHandler:            (envId 0) @3 line 8
#  9 Exception >> _pass:with:                 (envId 0) @4 line 18
#  10 Exception >> pass                        (envId 0) @2 line 14
#  11 [] in  RubyContext >> requireFileNamed:  (envId 0) @3 line 17
#  12 Exception >> _executeHandler:            (envId 0) @3 line 8
#  13 Exception >> _pass:with:                 (envId 0) @4 line 18
#  14 Exception >> pass                        (envId 0) @2 line 14
#  15 [] in  RubyFile >> load                  (envId 0) @3 line 19
#  16 Exception >> _executeHandler:            (envId 0) @3 line 8
#  17 Exception >> _pass:with:                 (envId 0) @4 line 18
#  18 Exception >> pass                        (envId 0) @2 line 14
#  19 [] in  RubyCompiler >> compileFileNamed:loadName: (envId 0) @7 line 46
#  20 Exception >> _executeHandler:            (envId 0) @3 line 8
#  21 GsFile >> userAction:onClient:with:with:with: (envId 0) @1 line 1
#  22 GsFile >> _openUsingGzip:                (envId 0) @10 line 19
#  23 GsFile >> open                           (envId 0) @2 line 8
#  24 GsFile class >> rubyOpen:mode:           (envId 0) @5 line 5
#  25 GsFile class >> open:::&                 (envId 1) @10 line 5
#  26 GsFile class >> open::                   (envId 1) @2 line 1
#  27 LogTailer >> initialize::                (envId 1) @18 line 8
#  28 LogTailer >> initialize*&                (envId 1) @4 line 1
#  29 Class >> new*&                           (envId 1) @5 line 4
#  30 Class >> new:*&                          (envId 1) @4 line 1
#  31 [] in  Builder >> use:*&                 (envId 1) @2 line 2
#  32 RubyProc >> call:                        (envId 1) @2 line 2
#  33 [] in  Builder >> to_app                 (envId 1) @2 line 4
#  34 Array >> inject:&                        (envId 1) @9 line 6
#  35 Builder >> to_app                        (envId 1) @16 line 4
#  36 Object >> _compileFile                   (envId 1) @109 line 95
#  37 [] in  RubyCompiler >> compileFileNamed:loadName: (envId 0) @8 line 31
#  38 ExecBlock >> ensure:                     (envId 0) @2 line 10
#  39 [] in  RubyCompiler >> compileFileNamed:loadName: (envId 0) @16 line 33
#  40 ExecBlock >> on:do:                      (envId 0) @2 line 53
#  41 RubyCompiler >> compileFileNamed:loadName: (envId 0) @4 line 39
#  42 [] in  RubyFile >> load                  (envId 0) @7 line 13
#  43 ExecBlock >> ensure:                     (envId 0) @2 line 10
#  44 [] in  RubyFile >> load                  (envId 0) @8 line 14
#  45 ExecBlock >> on:do:                      (envId 0) @2 line 53
#  46 RubyFile >> load                         (envId 0) @10 line 17
#  47 [] in  RubyContext >> requireFileNamed:  (envId 0) @2 line 14
#  48 RubyContext >> withFile:do:              (envId 0) @4 line 3
#  49 [] in  RubyContext >> requireFileNamed:  (envId 0) @3 line 14
#  50 ExecBlock >> on:do:                      (envId 0) @2 line 53
#  51 RubyContext >> requireFileNamed:         (envId 0) @17 line 15
#  52 Kernel >> require:                       (envId 1) @6 line 2
#  53 [] in  Kernel >> require:                (envId 1) @2 line 2
#  54 ExecBlock >> rescue:do:                  (envId 0) @2 line 59
#  55 Kernel >> require:                       (envId 1) @5 line 9
#  56 Object >> _compileFile                   (envId 1) @8 line 3
#  57 [] in  RubyCompiler >> compileFileNamed:loadName: (envId 0) @5 line 17
#  58 ExecBlock >> on:do:                      (envId 0) @2 line 53
#  59 [] in  RubyCompiler >> compileFileNamed:loadName: (envId 0) @7 line 19
#  60 ExecBlock >> ensure:                     (envId 0) @2 line 10
#  61 [] in  RubyCompiler >> compileFileNamed:loadName: (envId 0) @16 line 33
#  62 ExecBlock >> on:do:                      (envId 0) @2 line 53
#  63 RubyCompiler >> compileFileNamed:loadName: (envId 0) @4 line 39
#  64 [] in  RubyFile >> load                  (envId 0) @7 line 13
#  65 ExecBlock >> ensure:                     (envId 0) @2 line 10
#  66 [] in  RubyFile >> load                  (envId 0) @8 line 14
#  67 ExecBlock >> on:do:                      (envId 0) @2 line 53
#  68 RubyFile >> load                         (envId 0) @10 line 17
#  69 [] in  RubyContext >> runFileNamed:withARGV: (envId 0) @2 line 14
#  70 RubyContext >> withFile:do:              (envId 0) @4 line 3
#  71 RubyContext >> runFileNamed:withARGV:    (envId 0) @18 line 14
#  72 Executed Code                                      @5 line 3
#  73 System class >> _gsReturnToC             (envId 0) @1 line 1
#    [GsProcess 119803393]
#  topaz 1> frame 21
#  21 GsFile >> userAction:onClient:with:with:with: (envId 0) @1 line 1
#      receiver [109429505  GsFile]      aGsFile
#      actionName [5104385 sz:7  Symbol]   GsfOpen
#      onClient 2
#      arg1 [109427457  Pathname]    aPathname
#      arg2 [109427201 sz:1  String] r
#      arg3 false
#  topaz 1> 
#  
#  