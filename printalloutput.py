from subprocess import call
import os

tests = [os.path.join("tests/", test) for test in os.listdir("tests/")]

for test in tests:
    print "Testings : " + test
    print "Tokens :"
    print
    call (['./pmkn', '-t', test])
    print
    print "Ast :"
    print
    call (['./pmkn', '-a', test])
    print