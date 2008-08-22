#!/bin/sh

test -n "$1" && SVN_PROJECT_ID=$1
test -n "$1" || SVN_PROJECT_ID=default
TRAC_PROJECT_ID=$SVN_PROJECT_ID

# Before
test -d $HOME/tmp/svn_$SVN_PROJECT_ID && rm -rf $HOME/tmp/svn_$SVN_PROJECT_ID
test -d $HOME/tmp/trac_$TRAC_PROJECT_ID && rm -rf $HOME/tmp/trac_$TRAC_PROJECT_ID
sudo test -d /var/lib/svn/$TRAC_PROJECT_ID && sudo rm -rf /var/lib/svn/$TRAC_PROJECT_ID
sudo test -d /var/lib/trac/$TRAC_PROJECT_ID && sudo rm -rf /var/lib/trac/$TRAC_PROJECT_ID

# Extract
tar xzvf $HOME/svn_hotcopy/svn_$SVN_PROJECT_ID.tar.gz -C $HOME/tmp
tar xzvf $HOME/svn_hotcopy/trac_$TRAC_PROJECT_ID.tar.gz -C $HOME/tmp

# Restore
mkdir /var/lib/svn/$SVN_PROJECT_ID
sudo cp -av $HOME/tmp/tmp/svn_$SVN_PROJECT_ID /var/lib/svn/$SVN_PROJECT_ID/repos
sudo cp -av $HOME/tmp/tmp/trac_$TRAC_PROJECT_ID /var/lib/trac/$TRAC_PROJECT_ID
sudo mv /var/lib/svn/$SVN_PROJECT_ID/repos/dot_htpasswd /var/lib/svn/$SVN_PROJECT_ID/.htpasswd
sudo mv /var/lib/trac/$TRAC_PROJECT_ID/readme /var/lib/trac/$TRAC_PROJECT_ID/README
sudo mv /var/lib/trac/$TRAC_PROJECT_ID/version /var/lib/trac/$TRAC_PROJECT_ID/VERSION

# Permission
sudo chown -R svn:svn /var/lib/svn/$SVN_PROJECT_ID
sudo chown -R www-data:www-data /var/lib/trac/$TRAC_PROJECT_ID
sudo chmod -R 770 /var/lib/svn/$SVN_PROJECT_ID
sudo chmod -R 770 /var/lib/trac/$TRAC_PROJECT_ID

# After
test -d $HOME/tmp/tmp && rm -rf $HOME/tmp/tmp

