#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: writebits.st,v 1.5 2014-10-10 16:32:49-07 - - $"
execname := '$RCSfile: writebits.st,v $' =~ '.*: (.*),v.*' at: 1.

""
" Opens a file given as the first argument and writes out bits."
""

nl := Character nl.

FileStream extend [
   |bitNumber bitBuffer|
   clearByte [
      bitNumber := 8.
      bitBuffer := 0.
   ]
   flushByte [
      bitNumber = 8 ifFalse: [
         self nextPut: bitBuffer asCharacter.
         self clearByte.
      ]
   ]
   writeBit: bit [
      bitBuffer isNil ifTrue: [self clearByte].
      bitNumber = 0 ifTrue: [self flushByte].
      bitBuffer := bitBuffer bitAt: bitNumber put: bit.
      bitNumber := bitNumber - 1.
   ]
]


" Output of bitstream hello world."
bitArray := #(
   0 1 0 0   1 0 0 0
   0 1 1 0   0 1 0 1
   0 1 1 0   1 1 0 0
   0 1 1 0   1 1 0 0
   0 1 1 0   1 1 1 1
   0 0 1 0   1 1 0 0
   0 0 1 0   0 0 0 0
   0 1 0 1   0 1 1 1
   0 1 1 0   1 1 1 1
   0 1 1 1   0 0 1 0
   0 1 1 0   1 1 0 0
   0 1 1 0   0 1 0 0
   0 0 1 0   0 0 0 1
   0 0 0 0   1 0 1 0
).

writeBitArray := [:outfile|
   bitArray do: [:bit| outfile writeBit: bit].
   outfile flushByte.
].

Smalltalk arguments size = 0
   ifTrue: [writeBitArray value: stdout]
   ifFalse: [
      |outfilename|
      outfilename := Smalltalk arguments at: 1.
      [  |outfile|
         outfile := FileStream open: outfilename mode: FileStream write.
         writeBitArray value: outfile.
         outfile close.
      ] on: SystemExceptions.FileError do: [:signal |
         |errno|
         errno := File errno.
         stdout flush.
         stderr << execname << ': ' << filename << ': '
                         << signal messageText << nl.
         stderr flush.
      ]
   ]

"TEST: writebits.st"
"TEST: writebits.st | od -c"
"TEST: writebits.st /tmp/foo2"
"TEST: cat -nv /tmp/foo2"
"TEST: od -c /tmp/foo2"

