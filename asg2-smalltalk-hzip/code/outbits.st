#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: outbits.st,v 1.2 2012-03-06 22:33:59-08 - - $"
execname := '$RCSfile: outbits.st,v $' =~ '.*: (.*),v.*' at: 1.

""
" Opens a file given as the first argument and writes out bits."
""

nl := Character nl.

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      currentByte = 8 ifFalse: [
         myStream << currentByte asCharacter.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
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
   |outBytes|
   outBytes := OutBits new: outfile.
   bitArray do: [:bit| outBytes writeBit: bit].
   outBytes flushByte.
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

"TEST: outbits.st"
"TEST: outbits.st | od -c"
"TEST: outbits.st /tmp/foo1"
"TEST: cat -nv /tmp/foo1"
"TEST: od -c /tmp/foo1"

