#!/usr/bin/env python
# -*- coding: utf-8 -*-

import simplejson, urllib2, twitter, sys

username = 'twitt_'
password = 'xxxxxxxx'
watch_user = sys.argv[1]

auth_handler = urllib2.HTTPBasicAuthHandler() 
auth_handler.add_password('http', 'http://twitter.com/', username, password) 
opener = urllib2.build_opener(auth_handler)
api = twitter.Api(username, password)
success = 0
print 'start %s' % watch_user
for index in range(1,100):
  data = opener.open('http://twitter.com/statuses/friends/%s.json?page=%s' % (watch_user, index), {})
  data = simplejson.loads(data.read())
  if 0 == len(data):
    break
  print 'index %s' % index
  for user in data:
    name = user['screen_name']
    try:
      api.CreateFriendship(name)
      success += 1
      print 'ok %s' % name
    except:
      print 'error %s' % name
print 'finish new add %s' % success
