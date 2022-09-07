#!/usr/bin/env python

import os, sys
import argparse
from itertools import zip_longest

from PIL import Image

def grouper(iterable, n, fillvalue=None):
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)

def png2cube(inputFile, depth, outputFile, ascii):

    if not os.path.exists(inputFile):
        print(f'ERROR: File not found: {inputFile}')
        return False

    if depth not in [1, 2, 4, 8]:
        print(f'Depth must be 1, 2, 4, or 8 (got: {depth})')
        return False

    with Image.open(inputFile) as img:

        colors = img.getcolors()
        if not colors:
            print('Too many colors found in image')
            return False

        if depth == 1 and len(colors) > 2:
            print(f'Depth of 1 can have a maximum of 2 colors (found: {len(colors)})')
            return False
        elif depth == 2 and len(colors) > 4:
            print(f'Depth of 2 can have a maximum of 4 colors (found: {len(colors)})')
            return False
        elif depth == 4 and len(colors) > 16:
            print(f'Depth of 4 can have a maximum of 16 colors (found: {len(colors)})')
            return False
        elif depth == 8 and len(colors) > 256:
            print(f'Depth of 8 can have a maximum of 256 colors (found: {len(colors)})')
            return False

        #palette = img.getpalette()

        pixels = img.load()
        data = []
        for j in range(img.size[1]):
            for i in range(img.size[0]):
                colorIndex = pixels[i, j]
                data.append(colorIndex)

        # Pack data based on depth
        packedData = []
        if depth == 1:
            for group in grouper(data, 8):
                packedData.append((group[0] << 7) | 
                                  (group[1] << 6) |
                                  (group[2] << 5) |
                                  (group[3] << 4) |
                                  (group[4] << 3) |
                                  (group[5] << 2) |
                                  (group[6] << 1) |
                                  (group[7]))
        elif depth == 2:
            for group in grouper(data, 4):
                packedData.append((group[0] << 6) | 
                                  (group[1] << 4) |
                                  (group[2] << 2) |
                                  (group[3]))
        elif depth == 4:
            for group in grouper(data, 2):
                packedData.append((group[0] << 4) |
                                  (group[1]))
        else:
            packedData = list(data)

        if ascii:
            with open(outputFile, "w") as output:
                for block in grouper(packedData, 4):
                    output.write(f'    db ${block[0]:02x}, ${block[1]:02x}, ${block[2]:02x}, ${block[3]:02x}\n')
        else:
            with open(outputFile, "wb") as output:
                output.write(bytes(packedData))

        print(f'Wrote {len(packedData)} bytes.')

        return True


if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='png2cube (minicube64) Image Converter')
    parser.add_argument('input', help='The input indexed PNG file')
    parser.add_argument('-o', '--out', dest='output', type=str, help='The name of the output file', required=True)
    parser.add_argument('-d', '--depth', dest='depth', type=int, default=8, help='The bit depth of the image (1, 2, 4, 8)')
    parser.add_argument('-a', '--ascii', dest='ascii', action=argparse.BooleanOptionalAction, help='Write out data in ASM6 ASCII format')
    
    args = parser.parse_args()

    png2cube(args.input, args.depth, args.output, args.ascii)




