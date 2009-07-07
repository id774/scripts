#!/bin/sh

ls -all | awk '{ print $0; x += $5 } END{ print "Total: " x }'
