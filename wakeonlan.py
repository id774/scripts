#!/usr/bin/env python

########################################################################
# wakeonlan.py: Wake-on-LAN Magic Packet Sender
#
#  Description:
#  This script sends a Wake-on-LAN magic packet to a specified MAC address.
#  It's designed to wake up computers remotely by using their network interface.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.2 2024-02-10
#       Refactored for better testability. Added function decomposition and extensive comments for maintenance.
#  v1.1 2023-12-08
#       Removed Python version check
#  v1.0 2022-08-03
#       Initial release. Basic functionality for sending Wake-on-LAN magic packets.
#
#  Usage:
#  Run the script with a MAC address as the argument.
#  Example:
#      wakeonlan.py 00:11:22:33:44:55
#  This will send a magic packet to the specified MAC address.
#
########################################################################

import os
import socket
import sys
from traceback import print_exc

DEFAULT_PORT = 9

def usage():
    script_path = os.path.abspath(__file__)
    in_header = False
    with open(script_path, 'r', encoding='utf-8') as f:
        for line in f:
            if line.strip().startswith('#' * 10):
                if not in_header:
                    in_header = True
                    continue
                else:
                    break
            if in_header and line.startswith('#'):
                if line.startswith('# '):
                    print(line[2:], end='')
                else:
                    print(line[1:], end='')
    sys.exit(0)

def format_mac_address(mac):
    """Remove delimiters from a MAC address and convert to uppercase."""
    return mac.replace('-', '').replace(':', '').upper()

def create_magic_packet(mac):
    """Create a magic packet from a MAC address."""
    if len(mac) != 12:
        raise ValueError("Incorrect MAC address format")
    # Repeat the MAC address 16 times after 6 bytes of FF
    return bytes.fromhex('FF' * 6 + mac * 16)

def send_udp_broadcast(packet, port=DEFAULT_PORT):
    """Send a packet to the broadcast address using UDP."""
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
        s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
        s.sendto(packet, ('<broadcast>', port))

def send_magic_packet(addr):
    """Main function to format MAC address, create and send magic packet."""
    mac = format_mac_address(addr)
    packet = create_magic_packet(mac)
    print('[INFO] Sending magic packet to 255.255.255.255:{} with {}'.format(DEFAULT_PORT, addr))
    send_udp_broadcast(packet)


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    try:
        send_magic_packet(sys.argv[1])
    except Exception as e:
        print("[ERROR] {}".format(e), file=sys.stderr)
        print_exc()
