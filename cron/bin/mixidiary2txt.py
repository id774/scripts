#!/usr/bin/env python
# -*- coding: euc-jp -*-

"""
mixidiary2txt
utilitiy to convert mixi diary to text
Author : Hiroki Ohani<hiro@liris.org>
"""

import mixi
import sys, time


_USAGE_FORMAT = """usage:
    %s -u mixiid -p password [-s proxy] [-e encoding][-t friendmixiid] [-m maxitem] [-o outputfilename] [-n] [-h]
        u: mixi id
        p: mixi password
        e: charactor encoding for output. the default value is euc-jp
        t: mixi id to get diary entries. multiple value is ok
           ie. -t 0001 -t 0023
        o: file name to ouput.
        m: max item number to get. default value is 10
        n; not get content
        s: if you are behind proxy, set proxy
           exp. http://my.proxy.com:port/
        h: print this usage
"""
def main(argv):
    import getopt
    def usage(cmd):
        print _USAGE_FORMAT % (cmd)
        
    try:
        opts, args = getopt.getopt(argv[1:], "hnu:p:s:e:o:t:m:")
        proxy = None
        userid = None
        password = None
        encoding = "utf8"
        target = []
        maxitem = 30
        with_content = True
        fout = sys.stdout
        for (k, v) in opts:
            if k == "-h":
                usage(argv[0])
                return 0
            elif k == "-u":
                userid = v
            elif k == "-p":
                password = v
            elif k == "-s":
                proxy = v
            elif k == "-e":
                encoding = v
            elif k == "-t":
                target.append(v)
            elif k == "-m":
                maxitem = int(v)
            elif k == "-n":
                with_content = False
            elif k == "-o":
                fout = open(v, "w")
    except:
        usage(argv[0])
        return 1

    if not userid or not password:
        usage(argv[0])
        return 0
    
    m = mixi.MIXI(proxy)
    m.login(userid, password)
    if len(target) == 0:
        l = m.new_friend_diary(maxitem, with_content)
    else:
        l = []
        for t in target:
            l += m.friend_diary(t, with_content)
    for e in l:
        dt = e["date"]
        fout.write("#"*80 + "\n")
        fout.write(time.strftime("%Y/%m/%d %H:%M", dt) + "\n")
        fout.write(e["creator"].encode(encoding, "ignore") + "\n")
        fout.write(e["id"].encode(encoding, "ignore") + "\n")
        fout.write(e["title"].encode(encoding, "ignore") + "\n")
        fout.write(e["link"].encode(encoding, "ignore") + "\n")

        if e.has_key("content"):
            fout.write("-"*40 + "\n")
            fout.write(e["content"].encode(encoding, "ignore") + "\n")

        fout.write(e["link"].encode(encoding, "ignore") + "#write" + "\n")

if __name__ == "__main__":
    sys.exit(main(sys.argv))
