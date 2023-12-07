#!/usr/bin/env python

########################################################################
# png-info.py: PNG File Information Reader
#
#  Description:
#  This Python script reads basic information from PNG files, such as
#  width, height, bit depth, and color type. It supports handling
#  multiple files and uses glob patterns for file selection.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-08
#       Added Python version check to ensure the script runs on Python 3.6 or higher.
#  v1.1 2023-12-06
#       Refactored for clarity and improved error handling.
#  v1.0 2023-11-25
#       Initial release. Reads width, height, bit depth, and color type
#       information from PNG files.
#
#  Usage:
#  python png-info.py <file_pattern>
#  Example: python png-info.py *.png
#
########################################################################

import struct
import sys
import glob

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
    # Check if Python version is 3.6 or higher, exit if not
    if not (sys.version_info.major > 3 or (sys.version_info.major == 3 and sys.version_info.minor >= 6)):
        print("This script requires Python 3.6 or higher!")
        sys.exit(1)

    # Process each file matching the provided glob pattern
    for arg in sys.argv[1:]:
        for filename in glob.glob(arg):
            try:
                width, height, bit_depth, color_type_name = read_png_info(
                    filename)
                print(f"File: {filename}")
                print(f"Width:        {width:4d}")
                print(f"Height:       {height:4d}")
                print(f"Bit Depth:    {bit_depth:4d}")
                print(f"Color Type:   {color_type_name}")
                print("")
            except Exception as e:
                print(f"Error processing {filename}: {e}")
