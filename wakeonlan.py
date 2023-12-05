#!/usr/bin/env python

########################################################################
# Wake-on-LAN Magic Packet Sender
#
#  Description:
#  This script sends a Wake-on-LAN magic packet to a specified MAC address.
#  It's designed to wake up computers remotely by using their network interface.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2022-08-03
#       Initial release. Basic functionality for sending Wake-on-LAN magic packets.
#
# Usage:
#  Run the script with a MAC address as the argument.
#  Example:
#      python wakeonlan.py 00:11:22:33:44:55
#  This will send a magic packet to the specified MAC address.
#
########################################################################

import sys
import socket
import struct
from traceback import print_exc

DEFAULT_PORT = 9

def send_magic_packet(addr):
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
        s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)

        mac_ = addr.upper().replace("-", "").replace(":", "")
        if len(mac_) != 12:
            raise Exception("{} is not a hardware address and I could not resolve it as to an IP address.".format(addr))
        buf_ = b'f' * 12 + (mac_ * 20).encode()

        magicp = b''
        for i in range(0, len(buf_), 2):
            magicp += struct.pack('B', int(buf_[i:i + 2], 16))

        print('Sending magic packet to 255.255.255.255:{} with {}'.format(DEFAULT_PORT, addr))
        s.sendto(magicp, ('<broadcast>', DEFAULT_PORT))


if __name__ == '__main__':
    argsmin = 1
    version = (3, 0)
    if sys.version_info > (version):
        if len(sys.argv) > argsmin:
            try:
                send_magic_packet(sys.argv[1])
            except BaseException:
                print_exc()
        else:
            print("This program needs at least %(argsmin)s arguments" %
                  locals())
    else:
        print("This program requires python > %(version)s" % locals())

