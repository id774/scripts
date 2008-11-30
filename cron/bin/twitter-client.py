#!/usr/bin/env python

import os, sys
import urllib2
from xml.dom.minidom import parse
import re
import time
import pynotify

password = 'xxxxxxxx'
timeline_url ='http://twitter.com/statuses/friends_timeline.xml' 
last_id = 0

def notify(username, tmpfile):
  auth_handler = urllib2.HTTPBasicAuthHandler()
  auth_handler.add_password(
    'Twitter API', 'http://twitter.com/',
    username, password)
  opener = urllib2.build_opener(auth_handler)
  urllib2.install_opener(opener)

  tagText = lambda node, tagName: \
  node.getElementsByTagName(tagName)[0].firstChild.nodeValue
  print >>sys.stderr, time.strftime("[%H:%M:%S] "), "checking update"

  global last_id
  f = open(tmpfile,'r')
  last_id = int(f.read())
  f.close
  r = urllib2.Request(timeline_url)
  r.add_data("since_id="+str(last_id))
  e = parse(file=urllib2.urlopen(r))
  for s in reversed(e.getElementsByTagName("status")):
    if int(tagText(s, "id")) > last_id:
      screen_name = tagText(s, "screen_name")
      text = tagText(s, "text")
      source = tagText(s,"source")
      icon_url = tagText(s,"profile_image_url")
      show_notify(screen_name, text, source, icon_url)
      print >>sys.stdout, "%s: %s from %s" % (screen_name, text, source)
  last_id = int(tagText(s, "id"))
  print >>sys.stderr, time.strftime("[%H:%M:%S] "), "last_id is", last_id
  f = open(tmpfile,'w')
  f.write(str(last_id))
  f.close()

def show_notify(screen_name, text, source, icon_url):
  n = pynotify.Notification(screen_name,
      "%s from %s" % (text, source))
  text_url = re.match("^.*(http://.*).*$", text)

if __name__ == '__main__':
  from optparse import OptionParser
  usage = "usage: %prog [options]"
  parser = OptionParser(usage)
  parser.add_option("-u", "--username", dest="username",
                    help="your username")
  parser.add_option("-t", "--tmpfile", dest="tmpfile",
                    help="temporary file")
  (options, args) = parser.parse_args()
  pynotify.init("TwitterNotifier")
  if options.username and options.tmpfile:
    notify(options.username, options.tmpfile)
  else:
    parser.error("Incorrect options, Add -h option for help.")

