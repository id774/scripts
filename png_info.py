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
#  Version History:
#  v1.2 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#       Added help message display when no arguments are provided.
#  v1.1 2023-12-06
#       Refactored for clarity and improved error handling.
#  v1.0 2023-11-25
#       Initial release. Reads width, height, bit depth, and color type
#       information from PNG files.
#
#  Usage:
#  python png_info.py <file_pattern>
#  Example: python png_info.py *.png
#
########################################################################

import glob
import struct
import sys


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

def print_help():
    print("Usage: python png_info.py <file_pattern>")
    print("Example: python png_info.py *.png")
    print("Reads width, height, bit depth, and color type information from PNG files.")


if __name__ == "__main__":
    if len(sys.argv) == 1:
        print_help()
    else:
        # Process each file matching the provided glob pattern
        for arg in sys.argv[1:]:
            for filename in glob.glob(arg):
                try:
                    width, height, bit_depth, color_type_name = read_png_info(
                        filename)
                    print("File: {}".format(filename))
                    print("Width:        {:4d}".format(width))
                    print("Height:       {:4d}".format(height))
                    print("Bit Depth:    {:4d}".format(bit_depth))
                    print("Color Type:   {}".format(color_type_name))
                    print("")
                except Exception as e:
                    print("Error processing {}: {}".format(filename, e))
