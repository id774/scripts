#!/usr/bin/env python

import os, sys

def swap_extensions(dir, before, after):
    if before[:1] != '.':
        before = '.'+before

    thelen = -len(before)
    if after[:1] != '.':
        after = '.'+after

    for path, subdirs, files in os.walk(dir):
        for oldfile in files:
            if oldfile[thelen:] == before:
                oldfile = os.path.join(path, oldfile)
                newfile = oldfile[:thelen] + after
                os.rename(oldfile, newfile)
                print(oldfile + " -> " + newfile)

def main():
    if len(sys.argv) != 4:
        print("Usage: swapext dir before after")
        sys.exit(1)

    swap_extensions(sys.argv[1], sys.argv[2], sys.argv[3])

if __name__ == '__main__':
    main()

