#!/bin/sh

export SCRIPT_NAME=$TMP/create-munin-plugins.sh
export PLUGINS_DIR=/etc/munin/plugins

echo "#!/bin/sh">$SCRIPT_NAME
sudo munin-node-configure --shell>>$SCRIPT_NAME
chmod +x $SCRIPT_NAME
cd $PLUGINS_DIR
sudo $SCRIPT_NAME
rm $SCRIPT_NAME

sudo systemctl restart munin-node.service

