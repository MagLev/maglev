#!/bin/env python


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

ChangeTime = os.path.getctime
def setUpToDate():
    f = open(__file__)
    myContent = f.read()
    f.close()
    f = open(__file__, 'w')
    f.write(myContent)
    f.close()

# find out the last time we run this script
lastChanged = ChangeTime(__file__)
gitDirectory = os.path.dirname(__file__)

maglevHome = os.environ.get('maglev_home', gitDirectory)
if not '.git' in os.listdir(maglevHome):
    print('''I expected maglev home to be the git root.''')
    exit(2)

relativePathForChangedGsFiles = 'src/smalltalk'
gsFileDirectory = os.path.join(maglevHome, relativePathForChangedGsFiles)

print 'See what has changed in %s' % gsFileDirectory
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

    topazString2 = '''run
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
    errors = ['unknown command:', '*******']
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
        for i in range(3): # lines
            stop = output.rfind('\n', stop + 1)
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
    if errorStart == 0:
        setUpToDate()
        print 'Filein without known errors done!'
        
