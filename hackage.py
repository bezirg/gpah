import os
import sys
from os.path import join, isdir

from subprocess import call

def main():
    os.chdir(sys.argv[1])
    pwd = os.getcwd()
    hackage_contents = filter(isdir, os.listdir(pwd))
    for package in hackage_contents:
        package_versions = os.listdir(join(pwd, package))
        for package_version in package_versions:
            call(["tar", "-xzf", 
                            join(pwd, package, package_version,  "%s-%s.tar.gz" % (package, package_version)),
                            "-C", join(pwd, package, package_version)])

if __name__=="__main__":
    main()
