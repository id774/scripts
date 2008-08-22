#!/usr/bin/env python

# Your account information
username='twitt_'
password='xxxxxxxx'
# screen_resolution is used for balloon positioning
#screen_resolution=1280,1024
screen_resolution=1024,768 
# Minimum update_interval under Twitter API restriction is 60*60/70=51.4
update_interval=52
# Balloon will disapear after notify_timeout seconds
notify_timeout=2

import os
import sys
import urllib2
from xml.dom.minidom import parse
import re
import webbrowser
import time

import pygtk
pygtk.require('2.0')
import gtk
import gobject
import pynotify

tmpdir="/tmp/twitter-notifier/"
favicon_url = "http://assets3.twitter.com/images/favicon.ico"
update_url = "http://twitter.com/statuses/update.xml"
timeline_url ='http://twitter.com/statuses/friends_timeline.xml' 

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
  req.add_data("source=twnotifier&status="+text)
  res = parse(file=urllib2.urlopen(req))

last_id = 0
twits = []
def check_update():
  print >>sys.stderr, time.strftime("[%H:%M:%S] "), "checking update"
  gobject.timeout_add(update_interval*1000, check_update)

  r = urllib2.Request(timeline_url)
  global last_id
  r.add_data("since_id="+str(last_id))
  e = parse(file=urllib2.urlopen(r))
  for s in reversed(e.getElementsByTagName("status")):
    if last_id and int(tagText(s, "id")) > last_id:
      screen_name = tagText(s, "screen_name")
      text = tagText(s, "text")
      source = tagText(s,"source")
      icon_url = tagText(s,"profile_image_url")
      twits.append((screen_name, text, source, icon_url))
      #show_notify(screen_name, text, source, icon_url)
      print >>sys.stderr, "%s: %s from %s" % (screen_name, text, source)
  last_id = int(tagText(s, "id"))

user_page_tmpl = '<a href="http://twitter.com/%s/with_friends">%s</a>' 
last_notify=None
def show_notify():
  gobject.timeout_add(notify_timeout*1000+1000, show_notify)
  global twits

  if len(twits) == 0:
    return

  screen_name, text, source, icon_url = twits.pop(0)
  url = re.search('http://\S+', text, re.UNICODE)
  reply = re.search('@(\w+)', text)
  if url:
    url = url.group(0)
    a = '<a href="%s">%s</a>' % (url, url)
    text = text.replace(url, a)
  if reply:
    reply = reply.group(1)
    a = user_page_tmpl % (reply, reply)
    text = text.replace(reply, a)
  n = pynotify.Notification(screen_name, '%s from %s' % (text, source), get_icon(screen_name, icon_url))
  n.set_hint("x", screen_resolution[0]-50)
  n.set_hint("y", screen_resolution[1]-30)
  n.add_action("@%s " % screen_name, "Reply", edit_cb)
  n.add_action("http://twitter.com/%s/with_friends" % screen_name, "Browse", browse_url)
  n.set_timeout(notify_timeout*1000)
  n.show()

  global last_notify
  last_notify=n

def browse_url(n, url):
  webbrowser.open(url)

def get_icon(screen_name, icon_url):
  icon_url = icon_url.replace("s3.amazonaws.com:", "s3.amazonaws.com")

  if not os.access(tmpdir, os.F_OK):
    os.mkdir(tmpdir)

  filename = tmpdir+screen_name
  if not os.access(filename, os.F_OK):
    f = file(filename, "wb")
    f.write(urllib2.urlopen(icon_url).read())
  return filename

def quit_cb(widget, data = None):
  if data:
    data.set_visible(False)
  gtk.main_quit()

def last_cb(wigdet, data = None):
  global last_notify
  if last_notify:
    last_notify.show()

def edit_cb(widget, data = None):
  text = gtk.Entry()
  text.connect("activate", enter_cb)
  if data:
    text.set_text(data)
  win = gtk.Window()
  win.add(text)
  win.set_size_request(600,25)
  win.set_title("What are you doing?")
  win.set_position(gtk.WIN_POS_CENTER)
  text.show()
  win.show()

def enter_cb(widget, data = None):
  widget.parent.hide()
  update(widget.get_text())

def refresh_cb(widget, data = None):
  notify()

def home_cb(widget, data = None):
  browse_url(None, "http://twitter.com/home")

def popup_menu_cb(widget, button, time, data = None):
  if button == 3:
    if data:
      data.show_all()
      data.popup(None, None, None, 3, time)

def activate_icon_cb(widget, data = None):
  msgBox = gtk.MessageDialog(parent = None, buttons = gtk.BUTTONS_OK,
      message_format = "TwitterNotifier")
  msgBox.run()
  msgBox.destroy()

def main():
  pynotify.init("TwitterNotifier")

  statusIcon = gtk.StatusIcon()

  menu = gtk.Menu()

  menuItem = gtk.ImageMenuItem(gtk.STOCK_ABOUT)
  menuItem.connect('activate', activate_icon_cb)
  menu.append(menuItem)

  menuItem = gtk.ImageMenuItem(gtk.STOCK_HOME)
  menuItem.connect('activate', home_cb, statusIcon)
  menu.append(menuItem)
  
  menuItem = gtk.ImageMenuItem(gtk.STOCK_REFRESH)
  menuItem.connect('activate', refresh_cb, statusIcon)
  menu.append(menuItem)

  menuItem = gtk.ImageMenuItem(gtk.STOCK_UNDO)
  menuItem.connect('activate', last_cb, statusIcon)
  menu.append(menuItem)

  menuItem = gtk.ImageMenuItem(gtk.STOCK_EDIT)
  menuItem.connect('activate', edit_cb, statusIcon)
  menu.append(menuItem)

  menuItem = gtk.ImageMenuItem(gtk.STOCK_QUIT)
  menuItem.connect('activate', quit_cb, statusIcon)
  menu.append(menuItem)

  statusIcon.set_from_file(get_icon("twitter", favicon_url))
  statusIcon.set_tooltip("TwitterNotifier")
  statusIcon.connect('popup-menu', popup_menu_cb, menu)
  statusIcon.connect('activate', edit_cb, None)
  statusIcon.set_visible(True)

  gobject.timeout_add(1000, check_update)
  gobject.timeout_add(4000, show_notify)
  gtk.main()

if __name__ == '__main__':
  main()
