#!/usr/bin/env python

username="twitt"
password="xxxxxxxx"

import os, sys
import urllib2
from xml.dom.minidom import parse
import re
import time
import pynotify

tmpfile="/home/plagger/.plagger_tmp/last_id"
update_url = "http://twitter.com/statuses/update.xml"
timeline_url ='http://twitter.com/statuses/friends_timeline.xml' 
last_id = 0

auth_handler = urllib2.HTTPBasicAuthHandler()
auth_handler.add_password(
    'Twitter API', 'http://twitter.com/',
    username, password)
opener = urllib2.build_opener(auth_handler)
urllib2.install_opener(opener)

tagText = lambda node, tagName: \
  node.getElementsByTagName(tagName)[0].firstChild.nodeValue

def update(text):
  print >>sys.stderr, time.strftime("[%H:%M:%S] "), "updating"
  req = urllib2.Request(update_url)
  req.add_header("User-Agent", "TwitterNotifier http://yanbe.org/twitter-notifier/")
  req.add_header("X-Twitter-Client", "TwitterNotifier")
  req.add_header("X-Twitter-Client-URL", "http://yanbe.org/twitter-notifier/")
  req.add_header("X-Twitter-Client-Version", "0.1")
  req.add_data("status="+text)
  res = parse(file=urllib2.urlopen(req))

def notify():
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
  usage = "usage: %prog [options] arg"
  parser = OptionParser(usage)
  (options, args) = parser.parse_args()
  pynotify.init("TwitterNotifier")
  if len(args) != 1:
    notify()
  else:
    update(sys.argv[1])

