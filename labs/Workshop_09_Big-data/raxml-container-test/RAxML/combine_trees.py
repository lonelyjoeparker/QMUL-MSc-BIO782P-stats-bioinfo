import sys

print '#NEXUS'
print 'begin trees;'

for line in sys.stdin:
    print 'tree 1 = ' + line.rstrip()
    
print 'end;'   


