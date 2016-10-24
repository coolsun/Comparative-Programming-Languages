#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]"

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]
   
   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
       |((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

nl := Character nl.

freqtable := ZeroArray new: 256. "declaring array"
sortcol := SortedCollection new.
|tOn|
tOn := 0.
Smalltalk arguments size = 0
   ifTrue: [stdin catFile]
   ifFalse: [
      Smalltalk arguments do: [:filename |
         "stdout  << filename  << nl."

         "filename = '-t'
            ifTrue: [t0n := 1.
               stdout << 't option is ON' << nl.
            ]
            ifFalse: []."

         filename = '-'
            ifTrue: [stdin catFile]
            ifFalse: [
               [
                  |infile|
                  filename = '-t' 
                     ifTrue: [tOn := 1.
                        stdout << 't option is ON' << nl.
                     ]
                     ifFalse: [
                  infile := FileStream open: filename
                                       mode: FileStream read.
       
                  [infile atEnd not] whileTrue: [ "Stores in freq"
                     |ordChar|
                     ordChar := infile next asInteger.
                     freqtable incr: ordChar.
                  ].
                  
                  freqtable keysAndValuesDo: [:key :value | "Freq table debug"
                     (value notNil and: [value > 0]) ifTrue: [
                        key = 10
                           ifTrue:[
                           ]
                           ifFalse:[
                        "stdout << 'freq[' << key asCharacter visible << ']=' << value
                               << nl." "Prints out debug"
                        sortcol add: (Leaf new: key asCharacter count: value).
                           ]   
                     ]
                  ].
                  
                  
                  sortcol do: [:item| stdout << item << nl.]. "Prints out table in order"
                  
                  stdout nl.
                  "Loop creating tree"
                  [sortcol notEmpty] whileTrue: [
                     |left right newcount leftcount rightcount newtree newchar|
                     left := sortcol removeFirst.
                     leftcount := left count.
                     rightcount := 0.
                     newchar := left char.
                     sortcol notEmpty ifTrue: [
                        right := sortcol removeFirst.
                        rightcount := right count.
                     ].
                     newcount := leftcount + rightcount.
                     newtree := Tree new: newchar count: newcount
                                     left: left right: right.
                     sortcol notEmpty 
                        ifTrue: [sortcol add: newtree.]
                        ifFalse: [
                           newtree inorder: [:char :string |
                              stdout << '[' << char << ']=' << string << nl.
                           ] prefix: ''.
                        ].
                     stdout <<'left= ' << leftcount << ' right= ' <<
                          rightcount << ' newcount= ' << newcount << nl.
                  ].
                  tOn = 1
                     ifTrue: [
                        stdout << 'CONFIRM' << nl.
                     ].
                  infile close.
                  ]
               ] on: SystemExceptions.FileError do: [:signal |
                  |errno|
                  errno := File errno.
                  stdout flush.
                  stderr << execname << ': ' << filename << ': '
                         << signal messageText << nl.
                  stderr flush.
               ]
            ]
      ]
   ]
 









