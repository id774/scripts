#!/usr/bin/env python

########################################################################
# image_resize.py: Image Resizer
#
#  Description:
#  This Python script resizes images in a specified directory to a given size.
#  It supports JPEG and PNG formats. The script walks through the source directory,
#  resizes each image, and saves it to the output directory. This version includes
#  error handling for environments where the PIL library is not installed.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2024-01-11
#      Added error handling for PIL library not installed.
#  v1.0 2023-12-24
#      Initial release. Basic functionality for resizing images.
#
#  Usage:
#  python image_resize.py <size> <source_directory> <output_directory>
#  Example: python image_resize.py 300 ./images ./resized
#
########################################################################

import argparse
import os
import sys

# Importing PIL library and handling errors if not installed
try:
    from PIL import Image
except ImportError as e:
    libraries_installed = False
else:
    libraries_installed = True

def resize_file(size, filename, outpath):
    """Resizes an image to a square of the given size."""
    img = Image.open(filename, 'r')
    img.thumbnail((size, size))
    img.save(outpath)

def is_image_file(filename):
    """Checks if a file is a JPEG or PNG image using Pillow."""
    try:
        with Image.open(filename) as img:
            return img.format.lower() in ['jpeg', 'png']
    except IOError:
        return False

def read_dir(size, src, out):
    """Reads the source directory and resizes images found there."""
    for root, _, files in os.walk(src):
        for filename in files:
            fullname = os.path.join(root, filename)
            if is_image_file(fullname):
                outpath = os.path.join(out, filename)
                print("Resize: {0}".format(filename))
                resize_file(size, fullname, outpath)

def parse_args():
    """Parses command line arguments."""
    parser = argparse.ArgumentParser(
        description="Resize images in a directory.")
    parser.add_argument("size", type=int, help="Size of the output image.")
    parser.add_argument("src", help="Source directory of images.")
    parser.add_argument("out", help="Output directory for resized images.")
    return parser.parse_args()

def main():
    """Main function to execute the script."""
    if not libraries_installed:
        print("Error: Required libraries not installed.")
        sys.exit(1)

    args = parse_args()
    read_dir(args.size, args.src, args.out)


if __name__ == '__main__':
    main()
