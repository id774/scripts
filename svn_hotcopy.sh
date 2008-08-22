#!/bin/sh

test -n "$1" && SVN_PROJECT_ID=$1
test -n "$1" || SVN_PROJECT_ID=default
TRAC_PROJECT_ID=$SVN_PROJECT_ID

# Before
test -d $HOME/svn_hotcopy || mkdir $HOME/svn_hotcopy
test -d /tmp/svn_$SVN_PROJECT_ID && rm -rf /tmp/svn_$SVN_PROJECT_ID
test -d /tmp/trac_$TRAC_PROJECT_ID && rm -rf /tmp/trac_$TRAC_PROJECT_ID
test -f $HOME/svn_$SVN_PROJECT_ID.tar.gz && rm $HOME/svn_hotcopy/svn_$SVN_PROJECT_ID.tar.gz
test -f $HOME/trac_$TRAC_PROJECT_ID.tar.gz && rm $HOME/svn_hotcopy/trac_$TRAC_PROJECT_ID.tar.gz

# HotCopy
test -d /var/lib/svn/$SVN_PROJECT_ID/repos && svnadmin hotcopy /var/lib/svn/$SVN_PROJECT_ID/repos /tmp/svn_$SVN_PROJECT_ID
test -d /var/lib/trac/$TRAC_PROJECT_ID && trac-admin /var/lib/trac/$TRAC_PROJECT_ID hotcopy /tmp/trac_$TRAC_PROJECT_ID

# Passwd
test -f /var/lib/svn/$SVN_PROJECT_ID/.htpasswd && cp /var/lib/svn/$SVN_PROJECT_ID/.htpasswd /tmp/svn_$TRAC_PROJECT_ID/dot_htpasswd

# Archive
tar czvf $HOME/svn_hotcopy/svn_$SVN_PROJECT_ID.tar.gz /tmp/svn_$SVN_PROJECT_ID
tar czvf $HOME/svn_hotcopy/trac_$TRAC_PROJECT_ID.tar.gz /tmp/trac_$TRAC_PROJECT_ID

# After
test -d /tmp/svn_$SVN_PROJECT_ID && rm -rf /tmp/svn_$SVN_PROJECT_ID
test -d /tmp/trac_$TRAC_PROJECT_ID && rm -rf /tmp/trac_$TRAC_PROJECT_ID

