Halve sheet music PDFs for small screens.

Depends on:
- python
- haskell
- $ sudo apt-get install mupdf-tools python-pypdf libghc-juicypixels-dev

Install with:
$ make install

Run as:
$ halve-sheet-music.py bach.pdf output.pdf

Currently if there is a fatal error a temporary directory is left around,
with a /tmp/tmp* name. I'm fine with that for now. Beta!
