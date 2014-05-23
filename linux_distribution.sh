#!/bin/bash

ISSUE=`cat /etc/issue | head -n 1`
DIST=`echo $ISSUE | awk '{print $1}'`

if [[ $DIST =~ "CentOS" ]]; then
	VER=`echo $ISSUE | awk '{print $3}'`
elif [[ $DIST =~ "Scientific" ]]; then
	VER=`echo $ISSUE | awk '{print $4}'`
elif [[ $DIST =~ "Ubuntu" ]]; then
	VER=`echo $ISSUE | awk '{print $2}'`
elif [[ $DIST =~ "Debian" ]]; then
	VER=`echo $ISSUE | awk '{print $3}'`
fi

echo "Distribution: $DIST"
echo "Version: $VER"
