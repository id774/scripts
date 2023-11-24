#!/usr/bin/env python

import struct
import sys
import glob

def read_png_info(png_file_path):
    """Reads basic PNG information from the file."""
    with open(png_file_path, "rb") as f:
        # Read PNG file signature
        sign = f.read(8)
        if sign != b'\x89PNG\r\n\x1a\n':
            raise ValueError("Not a valid PNG file")

        # Read the IHDR chunk
        length = struct.unpack(">I", f.read(4))[0]
        chunk_type = f.read(4)
        if chunk_type != b'IHDR':
            raise ValueError("IHDR chunk not found")

        data = f.read(length)
        width, height, bit_depth, color_type = struct.unpack(">IIBB", data[:10])

        # Determine the name of the color type
        color_type_name = {
            3: "PNG-8",
            2: "PNG-24",
            6: "PNG-32"
        }.get(color_type, "UNKNOWN")

        return width, height, bit_depth, color_type_name

if __name__ == "__main__":
    for arg in sys.argv[1:]:
        for filename in glob.glob(arg):
            try:
                width, height, bit_depth, color_type_name = read_png_info(filename)
                print("File: {}".format(filename))
                print("Width:        {:4d}".format(width))
                print("Height:       {:4d}".format(height))
                print("Bit Depth:    {:4d}".format(bit_depth))
                print("Color Type:   {}".format(color_type_name))
                print("")
            except Exception as e:
                print("Error processing {}: {}".format(filename, e))

