#!/usr/bin/env python

########################################################################
# png_info.py: PNG File Information Reader
#
#  Description:
#  This Python script reads basic information from PNG files, such as
#  width, height, bit depth, and color type. It supports handling
#  multiple files and uses glob patterns for file selection.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      png_info.py <file_pattern>
#
#  Example:
#      png_info.py *.png
#  Reads width, height, bit depth, and color type information from PNG files.
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Version History:
#  v1.5 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.2 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#       Added help message display when no arguments are provided.
#  v1.1 2023-12-06
#       Refactored for clarity and improved error handling.
#  v1.0 2023-11-25
#       Initial release. Reads width, height, bit depth, and color type
#       information from PNG files.
#
########################################################################

import glob
import os
import struct
import sys


def usage():
    """ Display the script header as usage information and exit. """
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
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
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)

def read_png_info(png_file_path):
    """
    Reads basic information from a PNG file.

    Args:
        png_file_path (str): The path to the PNG file.

    Returns:
        tuple: Contains width, height, bit depth, and color type name of the PNG.

    Raises:
        ValueError: If the file is not a valid PNG or lacks essential data.
    """
    with open(png_file_path, "rb") as f:
        # Verify PNG file signature
        sign = f.read(8)
        if sign != b'\x89PNG\r\n\x1a\n':
            raise ValueError("Not a valid PNG file")

        # Read and process the IHDR chunk for image properties
        length, chunk_type = struct.unpack(">I4s", f.read(8))
        if chunk_type != b'IHDR':
            raise ValueError("IHDR chunk not found")

        data = f.read(length)
        width, height, bit_depth, color_type = struct.unpack(
            ">IIBB", data[:10])

        # Mapping of color type values to human-readable names
        color_types = {3: "PNG-8", 2: "PNG-24", 6: "PNG-32"}
        color_type_name = color_types.get(color_type, "UNKNOWN")

        return width, height, bit_depth, color_type_name


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    else:
        # Process each file matching the provided glob pattern
        for arg in sys.argv[1:]:
            for filename in glob.glob(arg):
                try:
                    width, height, bit_depth, color_type_name = read_png_info(
                        filename)
                    print("[INFO] File: {}".format(filename))
                    print("Width:        {:4d}".format(width))
                    print("Height:       {:4d}".format(height))
                    print("Bit Depth:    {:4d}".format(bit_depth))
                    print("Color Type:   {}".format(color_type_name))
                    print("")
                    sys.exit(0)
                except Exception as e:
                    print("[ERROR] Error processing {}: {}".format(filename, e), file=sys.stderr)
                    sys.exit(1)
