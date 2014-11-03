#!/usr/bin/env python

import copy
import math
import os
import pyPdf
import subprocess as spc
import sys

def hsplit_pages(path, src_fname, dst_fname):
    src_f = open(src_fname, "rb")
    dst_f = open(dst_fname, "wb")

    input = pyPdf.PdfFileReader(src_f)
    output = pyPdf.PdfFileWriter()

    # pyPdf errors on some of the files being blank encrypted. This is common?
    if input.isEncrypted:
        input.decrypt("")

    for i in range(input.getNumPages()):
    #for i in range(26, 28):
        p = input.getPage(i)
        q = copy.copy(p)
        q.mediaBox = copy.copy(p.mediaBox)

        x1, y1 = p.mediaBox.lowerLeft
        x2, y2 = p.mediaBox.upperRight

        pNumStr = str(i + 1)
        proc = spc.Popen(["png-halve-sheet-music",
            os.path.join(path, pNumStr + ".png"),
            src_fname + " page " + pNumStr + " (" + str(2 * i + 1) + "): "],
            stdout=spc.PIPE)
        (half1_new_btm_s, half2_new_top_s, _) = proc.stdout.read().split("\n")
        proc.wait()
        half1_new_btm = int(half1_new_btm_s)
        half2_new_top = int(half2_new_top_s)

        # pdf coordinate system is low-left origin:
        # vertically flipped from normal computer graphics
        p.mediaBox.lowerLeft = (x1, y2 - half1_new_btm)
        q.mediaBox.upperRight = (x2, y2 - half2_new_top)

        output.addPage(p)
        output.addPage(q)

    output.write(dst_f)
    src_f.close()
    dst_f.close()

def checksum(path):
    p = spc.Popen(["md5sum", path], stdout=spc.PIPE)
    r = p.stdout.read().split()[0]
    p.wait()
    return r

def wrap(src_fname, dst_fname, debug):
    path = os.path.join(os.environ["HOME"], ".config", "halve-sheet-music",
        checksum(src_fname))
    if not os.path.isdir(path):
        os.makedirs(path)
        spc.Popen(["mudraw", "-o", os.path.join(path, "%d.png"),
            src_fname]).wait()
    hsplit_pages(path, src_fname, dst_fname)

if __name__ == "__main__":
    debug = False
    args = sys.argv[1:]
    if args[0] == '--debug':
        debug = True
        args = args[1:]
    if len(args) != 2:
        print "usage: halve-sheet-music.py [--debug] bach.pdf output.pdf"
        exit(-1)
    (src_fname, dst_fname) = args
    if os.path.isdir(dst_fname):
        dst_fname = os.path.join(dst_fname, os.path.basename(src_fname))
    wrap(src_fname, dst_fname, debug)
