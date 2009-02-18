#!/usr/bin/env python

import sys, os

def hhk_switch(options, args):
    xmodmap_default = '/etc/xdg/xfce4/xmodmaprc'
    xmodmap_hhklite2 = '$SCRIPTS/dot_files/dot_xmodmaprc_hhklite2'

    if len(args) == 1:
        syscmd = 'xmodmap %s' % xmodmap_default
    else:
        syscmd = 'xmodmap %s' % xmodmap_hhklite2

    os.system(syscmd)

def main():
    from optparse import OptionParser
    usage = "usage: %prog [off]"
    parser = OptionParser(usage)
    (options, args) = parser.parse_args()
    hhk_switch(options, args)

if __name__ == "__main__":
    main()

