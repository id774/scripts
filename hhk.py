#!/usr/bin/env python

import sys, os

def hhk_switch(options, args):
    xmodmap_default = '/etc/xdg/xfce4/xmodmaprc'
    xmodmap_hhkprojp = '$SCRIPTS/dot_files/dot_xmodmaprc_hhkprojp'
    xmodmap_hhklite2 = '$SCRIPTS/dot_files/dot_xmodmaprc_hhklite2'

    if len(args) == 1:
        if args[0] == 'projp':
            syscmd = 'xmodmap %s' % xmodmap_hhkprojp
        elif args[0] == 'lite2':
            syscmd = 'xmodmap %s' % xmodmap_hhklite2
        else:
            syscmd = 'xmodmap %s' % xmodmap_default
    else:
        syscmd = 'xmodmap %s' % xmodmap_default

    os.system(syscmd)

def main():
    from optparse import OptionParser
    usage = "usage: %prog [off]"
    parser = OptionParser(usage)
    (options, args) = parser.parse_args()
    hhk_switch(options, args)

if __name__ == "__main__":
    main()

