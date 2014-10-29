#!/usr/bin/env python

import copy
import math
import os
import pyPdf
import shutil
import subprocess as spc
import sys
import tempfile

def hsplit_pages(tmpd, src_fname, dst_fname):
    src_f = file(src_fname, "r+b")
    dst_f = file(dst_fname, "w+b")

    input = pyPdf.PdfFileReader(src_f)
    output = pyPdf.PdfFileWriter()

    for i in range(input.getNumPages()):
    #for i in range(26, 28):
        p = input.getPage(i)
        q = copy.copy(p)
        q.mediaBox = copy.copy(p.mediaBox)

        x1, y1 = p.mediaBox.lowerLeft
        x2, y2 = p.mediaBox.upperRight

        #print(x1, y1)
        #print(x2, y2)

        proc = spc.Popen(["png-halve-sheet-music",
            os.path.join(tmpd, str(i + 1) + ".png")],
            stdout=spc.PIPE)
        (y_new1, y_new2, _) = proc.stdout.read().split("\n")
        proc.wait()
        #print y_new1
        #print y_new2
        y_new1 = int(y_new1)
        y_new2 = int(y_new2)

        p.mediaBox.lowerLeft = (x1, y_new1)
        q.mediaBox.upperRight = (x2, y_new2)

        output.addPage(p)
        output.addPage(q)

    output.write(dst_f)
    src_f.close()
    dst_f.close()

def wrap(src_fname, dst_fname):
    tmpd = tempfile.mkdtemp()
    proc = spc.Popen(["mudraw", "-o", os.path.join(tmpd, "%d.png"), src_fname])
    proc.wait()
    hsplit_pages(tmpd, src_fname, dst_fname)
    shutil.rmtree(tmpd)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print "usage: halve-sheet-music.py bach.pdf output.pdf"
        exit(-1)
    (_, src_fname, dst_fname) = sys.argv
    if os.path.isdir(dst_fname):
        dst_fname = os.path.join(dst_fname, os.path.basename(src_fname))
    wrap(src_fname, dst_fname)
