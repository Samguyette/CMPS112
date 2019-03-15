#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: catfile.st,v 1.2 2019-02-05 12:36:09-08 - - $"

"Partner: Sergey Gasparyan (sgaspary@ucsc.edu)"
"Partner: Samuel Guyette (sguyette@ucsc.edu)"

execname := '$RCSfile: catfile.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.

FileStream extend [ |bitIndex currentChar|
    nextBit [ |bit|
       bitIndex isNil ifTrue: [bitIndex := 0].
       bitIndex = 0 ifTrue: [
            bitIndex := 8.
            currentChar := self next.
       ].
       bit := currentChar value bitAt: bitIndex.
       bitIndex := bitIndex - 1.
       ^ bit
    ]

    atBitEnd[
       ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
    ]
]

Object subclass: OutBits [|bitIndex currentByte myStream|
    OutBits class >> new [
       self shouldNotImplement.
    ]
    OutBits class >> new: fileStream [|result|
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
        bitIndex = 8 ifFalse: [
           myStream nextPutByte: currentByte.
           self clearByte.
        ]
    ]
    writeBit: bit [
        currentByte := currentByte bitAt: bitIndex put: bit.
        bitIndex := bitIndex - 1.
        bitIndex = 0 ifTrue: [self flushByte].
    ]
]

Character extend [
    isGraph[
       ^ (Character space < self) & (self <= $~)
    ]
    visible [
       self isGraph ifTrue: [^ '$', self asString]
                    ifFalse: [^ self asInteger printStringRadix: 16]
       ]
]

Object subclass: BitStream [|bitIndex byte myStream|
    BitStream class >> new: fileStream [
        |result|
        result := super new.
        result init: fileStream.
        ^result
    ]
    init: fileStream [
        myStream := fileStream.
        bitIndex := 1.
    ]
    nextBit [
        bitIndex = 1 ifTrue: [
        byte := myStream next.
        bitIndex := 9.
        ].
        bitIndex := bitIndex - 1.
        ^byte value bitAt: bitIndex
    ]
    atEnd [
        ^bitIndex = 1 and: [myStream atEnd]
    ]
    currByte [
        ^byte
    ]
]

Object subclass: ZeroArray [|theArray|
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
        | ((count = other count) & (char <= other char))
    ]
    printBase: aStream [
        ^ aStream << self class << ')' << char << ',' << count
    ]
    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]
    depthFirst: visitor prefix: string [
        visitor value: char value: string.
    ]
    postorder: visitor [
        visitor value: char value: 0.
    ]
]


Leaf subclass: Tree [
    |left right|
    left [ ^ left ]
    right [ ^ right ]
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

    depthFirst: visitor prefix: string [
        left depthFirst: visitor prefix: string, '0'.
        right depthFirst: visitor prefix: string, '1'.
    ]

    postorder: visitor [
        left postorder: visitor.
        right postorder: visitor.
        visitor value: self char value: 1.
    ]
]

Smalltalk arguments: '-d -c -t -u'

do: [:opt : arg |
    (opt = $d) ifTrue: [
       stdout << 'There is no debugging information.' << nl.
    ].
    ((opt = $c) | (opt = $t)) ifTrue: [
           |infile outfile infileFileStream outfileFileStream
             frequenceTab finishedArray sortObj|
           infile := Smalltalk getArgv: 2.
           infileFileStream := FileStream open: infile.
           outfile := Smalltalk getArgv: 3.
           (outfile isNil) ifTrue: [
                outfileFileStream := stdout.
           ] ifFalse: [
                outfileFileStream := FileStream open: outfile.
           ].
           frequenceTab := ZeroArray new: 257.
           frequenceTab at: 256 put: 1.
           finishedArray := ZeroArray new: 257.

           [infileFileStream atEnd] whileFalse: [
                |currChar|
                currChar := infileFileStream next asInteger.
                frequenceTab incr: currChar.
                c := frequenceTab at: currChar.
           ].
           sortObj := SortedCollection new.
           frequenceTab keysAndValuesDo: [ :currChar :currCount |
                (currCount notNil and: [currCount > 0]) ifTrue: [
                      sortObj add: (Leaf new:
                       currChar asCharacter count: currCount).
                ]
           ].
           [sortObj size = 1] whileFalse: [
                |leafLeaf rightLeaf treeCount|
                leftLeaf := sortObj removeFirst.
                rightLeaf := sortObj removeFirst.
                treeCount := leftLeaf count + rightLeaf count.
                sortObj add: (Tree new: (leftLeaf char) count:
                 treeCount left: leftLeaf right: rightLeaf).
           ].

           finishedTree := sortObj first.
           finishedTree depthFirst: [:count :char |
                i := count asInteger.
                finishedArray at: i put: char.
           ] prefix: ''.

           (opt = $t) ifTrue: [
               finishedArray keysAndValuesDo: [:charNum :binaryValue |
                  (binaryValue notNil) ifTrue: [
                      (binaryValue > 0) ifTrue: [
                          |count space gap ascii|
                          count := (frequenceTab at: charNum).
                          (count < 10) ifTrue: [
                               space := '      '.
                               gap := '     '.
                          ].
                          (count >= 10) ifTrue: [
                               space := '     '.
                               gap := '    '.
                          ].
                          (count >= 100) ifTrue: [
                               space := '    '.
                               gap := '   '.
                          ].
                          (count >= 1000) ifTrue: [
                               space := '   '.
                               gap := '  '.
                          ].
                          (count >= 10000) ifTrue: [
                                space := '  '.
                                gap := ' '.
                          ].
                          (count >= 100000) ifTrue: [
                                space := ' '.
                               gap:= ''.
                          ].
                          (count >= 1000000) ifTrue: [
                                space := ''.
                          ].
                          ((charNum = 10) | (charNum = 256) |
                                (charNum = 32)) ifFalse: [
                             ascii := charNum asCharacter asString.
                             outfileFileStream  << ' ' << ascii
                             << space << count << ' ' <<
                             binaryValue << nl.
                          ].
                          (charNum = 10) ifTrue: [
                               outfileFileStream  << 'x0A' << gap
                                 << count << ' ' << binaryValue << nl.
                          ].
                          (charNum = 32) ifTrue: [
                               outfileFileStream << 'x20' << gap
                                  << count << ' ' << binaryValue << nl.
                          ].
                          (charNum = 256) ifTrue: [
                               outfileFileStream << 'EOF' << '     '
                                   << '1' << ' ' << binaryValue << nl.
                          ].
                      ].
                  ].
              ].
           ].
           (opt = $c) ifTrue: [
                |finishedTree displayBit|
                infileFileStream reset.
                displayBit := OutBits new: outfileFileStream.
                finishedTree := sortObj first.
                finishedTree postorder: [:char :bit |
                      |chr|
                   (bit = 1) ifTrue: [
                      displayBit writeBit: 1.
                   ] ifFalse: [
                      displayBit writeBit: 0.
                      chr := char asInteger.
                      8 to: 1 by: -1 do: [: i |
                         displayBit writeBit: (chr bitAt: i).
                      ].
                      (chr = 256) ifTrue: [
                         displayBit writeBit: 1.
                      ].
                      (chr = 0) ifTrue: [
                         displayBit writeBit: 0.
                      ].
                   ].
               ].
               displayBit writeBit: 1.
               [infileFileStream atEnd] whileFalse: [
                    |currChar binary bit|
                  currChar := infileFileStream next asInteger.
                  binary := finishedArray at: currChar.
                  1 to: (binary size) do: [:i |
                      bit := binary at: i.
                      displayBit writeBit: (bit asString asInteger).
                  ].
              ].
              (infileFileStream atEnd not) ifFalse: [
                     |bin bit|
                 bin := finishedArray at: 256.
                 1 to: (bin size) do: [ :i |
                     bit := bin at: i.
                     displayBit writeBit: (bit asString asInteger).
                 ].
                 displayBit flushByte.
             ].
         ].
         infileFileStream close.
         outfileFileStream close.
     ].
     (opt = $u) ifTrue: [
         |treeNode stack getBit infile flag treeHead outfile
          outfileFileStream infileFileStream |
         infile := Smalltalk getArgv: 2.
         outfile := Smalltalk getArgv: 3.
         (outfile isNil) ifTrue: [
              outfileFileStream := stdout.
         ] ifFalse: [
              outfileFileStream := FileStream open: outfile.
         ].
         infileFileStream := FileStream open: infile.
         getBit := BitStream new: infileFileStream.
         stack := OrderedCollection new.
         flag := true.

         [(getBit atEnd not) and: [flag]] whileTrue: [
              |bit byte left right tree leaf|
              byte := 0.
              bit := getBit nextBit.
              (bit = 0) ifTrue: [
                   8 to: 1 by: -1 do: [:i |
                        bit := getBit nextBit.
                        byte := byte bitAt: i put: bit.
                   ].
                   (byte = 0) ifTrue: [
                        (getBit nextBit = 1) ifTrue: [
                             byte := byte + 256.
                        ].
                   ].
                   leaf := (Leaf new: byte asCharacter count: -99).
                   stack addFirst: leaf.
               ] ifFalse: [
                   (stack size < 2) ifTrue: [
                        flag := false.
                   ] ifFalse: [
                        right := stack removeFirst.
                        left := stack removeFirst.
                        tree := Tree new: (left char) count: 0
                              left: left right: right.
                        stack addFirst: tree.
                   ].
               ].
         ].
         treeNode := stack removeFirst.
         treeHead := treeNode.
         flag := false.
         "if file is empty"
         (treeNode count) = -99 ifTrue: [
              flag := true.
         ].
         [(getBit atEnd not) and: [flag = false]] whileTrue: [
              |isleaf chr bit|
               bit := getBit nextBit.
               (bit = 1) ifTrue: [
                    treeNode := treeNode right.
               ] ifFalse: [
                    treeNode := treeNode left.
               ].
               isleaf := treeNode count.
               (isleaf = -99) ifTrue: [ "node is a leaf"
                   chr := (treeNode char).
                  chr asInteger = 256 ifFalse: [
                        outfileFileStream << chr.
                   ] ifTrue:[
                        flag := true. "we hit eof"
                   ].
                   treeNode := treeHead.
               ].
         ].
         infileFileStream close.
         outfileFileStream close.
     ].
].
