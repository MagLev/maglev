#!/usr/bin/env python


import os
import sys
import subprocess

try:
    __file__
except NameError:
    __file__ = ''
    i = 0
    while not __file__.endswith("filein.py"):
        if len(sys.argv) > i:
            __file__ = sys.argv[i]
        else:
            print('''You unusually execute me!
I expected my filename to be in the arguments passed to me..
would you do me this favor?''')
            exit(1)

if subprocess.call('hash topaz', shell = True) != 0:
    print('topaz must be callable from the commandline.')
    exit(10)

ChangeTime = os.path.getctime
def setUpToDate():
    f = open(__file__)
    myContent = f.read()
    f.close()
    f = open(__file__, 'w')
    f.write(myContent)
    f.close()
update = False
reloadPrimitivesAtTheEnd = False
showOutput = len(sys.argv) >= 2 and 'o' in sys.argv[1]
showHelp = len(sys.argv) >= 2 and 'h' in sys.argv[1]
if showHelp:
    print '''
h
    for help
o
    to show the output of topaz
'''
    

# find out the last time we run this script
lastChanged = ChangeTime(__file__)
gitDirectory = os.path.dirname(__file__)

maglevHome = os.environ.get('MAGLEV_HOME', gitDirectory)
if not '.git' in os.listdir(maglevHome):
    print('''I expected maglev home to be the git root.''')
    exit(2)

relativePathForChangedGsFiles = 'src/smalltalk'
gsFileDirectory = os.path.join(maglevHome, relativePathForChangedGsFiles)

print('Changes  in %s' % gsFileDirectory)
changedGsFiles = []
for dirPath, dirNames, fileNames in os.walk(gsFileDirectory):
    for fileName in fileNames:
        if fileName.endswith('.gs'):
            # can be filed in
            filePath = os.path.join(dirPath, fileName)
            if lastChanged < ChangeTime(filePath) + 10:
                print('\t%s' % fileName)
                changedGsFiles.append(filePath)
                
if not changedGsFiles:
    print('Nothing!')
else:
    topazString = '''set gemstone maglev user SystemUser pass swordfish
login
'''

    for filePath in changedGsFiles:
        topazString += '''printit
'InPuT_FiLe: {filePath}'
%

input {filePath}
'''.format(filePath = filePath)

    topazString += '''run
System commitTransaction
%
exit
'''

    pipe = subprocess.Popen( ['topaz'], \
                             stdout=subprocess.PIPE, \
                             stderr = subprocess.PIPE, \
                             stdin = subprocess.PIPE, \
                             shell = True)
    output, error = pipe.communicate(topazString)
    if showOutput:
        print('#' * 60)
        print(' topaz output     '.center(60, '#'))
        print('#' * 60)
        print(output)
        print('#' * 60)
        print(' topaz output end '.center(60, '#'))
        print('#' * 60)
    errors = ['unknown command:', '*****', '\nerror', '\nError', \
              'GemStone Smalltalk Compiler Errors:', 'WARNING:']
    errorStart = 0
    while 1:
        lastError = len(output)
        errorFound = False
        for error in errors:
            indexOfError = output.find(error, errorStart)
            if indexOfError < lastError and errorStart < indexOfError:
                lastError = indexOfError
                errorFound = True
        if not errorFound:
            break
        errorStart = lastError
        del lastError, indexOfError
        ## error output +- 3 lines
        start = errorStart
        for i in range(3): # lines
            start = output.rfind('\n', 0, start)
        stop = errorStart
        for i in range(5): # lines
            stop = output.find('\n', stop + 1)
        ## find the name of the bad file
        fileNameIndex = output.rfind('InPuT_FiLe: ', 0, errorStart)
        fileNameEndIndex = output.find('\n', fileNameIndex, errorStart)
        errorFileName = output[fileNameIndex + len('InPuT_FiLe: ') :
                               fileNameEndIndex]
        ## print error results
        print('-' * 50)
        print('An error occurred in %s' % errorFileName)
        print(output[start: stop])

        ## next error please!
        errorStart += 1
    if errorStart != 0:
        exit(1)
    update = True
    print('Filein without known errors done!')
    reloadPrimitivesAtTheEnd = True

## reload primitives
rbFileDirectory = os.path.join(maglevHome, 'src/kernel/bootstrap')
print('Changes  in %s' % rbFileDirectory)

for dirPath, dirNames, fileNames in os.walk(rbFileDirectory):
    for fileName in fileNames:
        if fileName.endswith('.rb'):
            # can be filed in
            filePath = os.path.join(dirPath, fileName)
            if lastChanged < ChangeTime(filePath) + 10:
                print('\t%s' % fileName)
                reloadPrimitivesAtTheEnd = True

if reloadPrimitivesAtTheEnd:
    print('Reload primitives.')
    pipe = subprocess.Popen(['rake maglev:reload_prims'],
                            stdout = subprocess.PIPE, \
                            shell = True)
    stdout, _stderr = pipe.communicate()
    if pipe.wait() != 0:
        print('-' * 50)
        print('rake maglev:reload_prims exited with status %i' % \
              pipe.wait())
        print stdout
        exit(pipe.wait())
    update = True
else:
    print('Nothing!')
      
if update:
    setUpToDate()
