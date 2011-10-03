#!/bin/sh -e
#
ifconfig | mail -s "[admin-log][`/bin/hostname`] This is test mail" idnanashi@gmail.com
