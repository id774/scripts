#!/bin/sh

export SCRIPT_NAME=$TMP/cleanup-removed-packages.sh

aptitude search . | grep '^c' | awk '{print $2}' | sed 's/^/sudo apt purge -y /g' > $SCRIPT_NAME
sed -i '1s/^/#!\/bin\/sh\n/' $SCRIPT_NAME
chmod +x $SCRIPT_NAME
$SCRIPT_NAME
rm $SCRIPT_NAME

