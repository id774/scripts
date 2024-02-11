#!/usr/bin/env python

########################################################################
# smb.py: Mount Network Shares in Python
#
#  Description:
#  This script provides a command-line interface to mount network shares
#  using SMB/CIFS protocol in different operating systems (Windows and POSIX).
#  It supports specifying the mount point, share name, IP address, username,
#  and password through command-line options.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-06
#      Initial version ported from Ruby to Python. Added compatibility for
#      both Windows and POSIX systems. Implemented command-line options for
#      specifying parameters.
#
#  Usage:
#  To mount a network share:
#      python smb.py -m <mount_point> -s <share_name> -i <ip_address> -u <username> -p <password>
#
#  Options:
#  -m, --mount  : Specify the mount point (default: ~/mnt)
#  -s, --share  : Specify the share name (default: homes)
#  -i, --ipaddr : Specify the IP address of the network share
#  -u, --user   : Specify the username for access
#  -p, --pass   : Specify the password for access
#
#  Example:
#      python smb.py -i 192.168.1.100 -u myUser -p myPass
#
#  Notes:
#  Ensure that the necessary permissions and network configurations are set
#  to allow mounting of network shares. This script may require administrative
#  privileges or sudo access, depending on the system's configuration.
#
########################################################################

import logging
import os
import subprocess
from optparse import OptionParser

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ExecOnWin:
    def run(self, mount, share, ipaddr, user, passw):
        bs = "\\\\"
        target = bs + bs + ipaddr + bs + share
        if self.ping(ipaddr):
            cmd = "net use {} {} {} /USER:{} /persistent:no".format(
                mount, target, passw, user)
            logger.info(subprocess.getoutput(cmd))
        else:
            logger.warning("{} ping timeout.".format(ipaddr))

    def ping(self, ipaddr):
        return os.system("ping -n 1 -w 3 {} > nul".format(ipaddr)) == 0

class ExecOnPosix:
    def run(self, mount, share, ipaddr, user, passw):
        if self.ping(ipaddr):
            cmd = "sudo mount -t cifs -o rw,uid={},username={},password={},iocharset=utf8 //{}/{} {}".format(
                user, user, passw, ipaddr, share, mount)
            logger.info(subprocess.getoutput(cmd))
        else:
            logger.warning("{} ping timeout.".format(ipaddr))

    def ping(self, ipaddr):
        return os.system("ping -c 1 {} > /dev/null".format(ipaddr)) == 0

def main():
    parser = OptionParser(usage="%prog [options]", version="%prog 1.0")
    parser.add_option("-m", "--mount", dest="mount",
                      default="~/mnt", help="mount (default = ~/mnt)")
    parser.add_option("-s", "--share", dest="share",
                      default="homes", help="share (default = homes)")
    parser.add_option("-i", "--ipaddr", dest="ipaddr", help="IP address")
    parser.add_option("-u", "--user", dest="user", help="username")
    parser.add_option("-p", "--pass", dest="passw", help="password")
    options, args = parser.parse_args()

    if not options.ipaddr:
        parser.error("IP address not specified")
        return

    if os.name == 'nt':
        executor = ExecOnWin()
    else:
        executor = ExecOnPosix()

    executor.run(options.mount, options.share,
                 options.ipaddr, options.user, options.passw)


if __name__ == "__main__":
    main()
