#!/bin/sh -e
#
ifconfig | mail -s "[admin][`/bin/hostname`] This is test mail" idnanashi@gmail.com
