Unit MZLib;

// Original copyright of the creators:
//
// zlib.H -- interface of the 'zlib' general purpose compression library version 1.1.0, Feb 24th, 1998
//
// Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler
//
// This software is provided 'as-is', without any express or implied warranty.  In no event will the authors be held
// liable for any damages arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter
// it and redistribute it freely, subject to the following restrictions:
// 1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software.
//    If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is
//    not required.
// 2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
// 3. This notice may not be removed or altered from any Source distribution.
//
// Jean-loup Gailly        Mark Adler
// jloup@gzip.org          madler@alumni.caltech.edu
//
// The data format used by the zlib library is described by RFCs (Request for Comments) 1950 to 1952 in the files
// ftp://deststate.internic.net/rfc/rfc1950.txt (zlib format), rfc1951.txt (Deflate format) and rfc1952.txt (gzip format).
//
// patch 112 from the zlib home page is implicitly applied here
//
// Delphi translation: (C) 2000 by Dipl. Ing. Mike Lischke (www.delphi-gems.com)

Interface

Uses
  Windows;
  
// The 'zlib' compression library provides in-memory compression and decompression functions, including integrity checks
// of the uncompressed data. This version of the library supports only one compression method (deflation) but other
// algorithms will be added later and will have the same stream interface.
//
// Compression can be done in a single step if the buffers are large enough (for example if an input file is mmap'ed),
// or can be done by repeated calls of the compression function. In the latter case, the application must provide more
// input and/or consume the output (providing more output space) before each call.
//
// The library also supports reading and writing files in gzip (.gz) format.
//
// The library does not install any signal handler. The decoder checks
// the consistency of the compressed data, so the library should never
// crash even in case of corrupted input.

//----------------- general library stuff ------------------------------------------------------------------------------

ResourceString
  SNeedDict = 'need dictionary';
  SStreamEnd = 'stream end';
  SFileError = 'file error';
  SStreamError = 'stream error';
  SDataError = 'data error';
  SInsufficientMemory = 'insufficient memory';
  SBufferError = 'buffer error';
  SIncompatibleVersion = 'incompatible version';
  SInvalidDistanceCode = 'invalid distance code';
  SInvalidLengthCode = 'invalid literal/length code';
  SOversubscribedDBLTree = 'oversubscribed dynamic bit lengths tree';
  SIncompleteDBLTree = 'incomplete dynamic bit lengths tree';
  SOversubscribedLLTree = 'oversubscribed literal/length tree';
  SIncompleteLLTree = 'incomplete literal/length tree';
  SEmptyDistanceTree = 'empty distance tree with lengths';
  SInvalidBlockType = 'invalid block type';
  SInvalidStoredBlockLengths = 'invalid stored block lengths';
  STooManyLDSymbols = 'too many length or distance symbols';
  SInvalidBitLengthRepeat = 'invalid bit length repeat';
  SIncorrectDataCheck = 'incorrect data check';
  SUnknownCompression = 'unknown compression method';
  SInvalidWindowSize = 'invalid window size';
  SIncorrectHeaderCheck = 'incorrect header check';
  SNeedDictionary = 'need dictionary';

Type
  PWord = ^Word;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;

Type
  TByteArray = Array[0..(MaxInt Div SizeOf(Byte)) - 1] Of Byte;
  PByteArray = ^TByteArray;

  TWordArray = Array[0..(MaxInt Div SizeOf(Word)) - 1] Of Word;
  PWordArray = ^TWordArray;

  TIntegerArray = Array[0..(MaxInt Div SizeOf(Integer)) - 1] Of Integer;
  PIntegerArray = ^TIntegerArray;

  TCardinalArray = Array[0..(MaxInt Div SizeOf(Cardinal)) - 1] Of Cardinal;
  PCardinalArray = ^TCardinalArray;

Const
  // maximum value for MemLevel in DeflateInit2
  MAX_MEM_LEVEL = 9;
  DEF_MEM_LEVEL = 8;

  // maximum value for WindowBits in DeflateInit2 and InflateInit2
  MAX_WBITS = 15; // 32K LZ77 window

  // default WindowBits for decompression, MAX_WBITS is for compression only
  DEF_WBITS = MAX_WBITS;

Type
  PInflateHuft = ^TInflateHuft;
  TInflateHuft = Record
    Exop,           // number of extra bits or operation
    Bits: Byte;     // number of bits in this code or subcode
    Base: Cardinal; // literal, Length base, or distance base or table offset
  End;

  THuftField = Array[0..(MaxInt Div SizeOf(TInflateHuft)) - 1] Of TInflateHuft;
  PHuftField = ^THuftField;
  PPInflateHuft = ^PInflateHuft;

  TInflateCodesMode = ( // waiting for "I:"=input, "O:"=output, "X:"=nothing
    icmStart,    // X: set up for Len
    icmLen,      // I: get length/literal/eob next
    icmLenNext,  // I: getting length extra (have base)
    icmDistance, // I: get distance next
    icmDistExt,  // I: getting distance extra
    icmCopy,     // O: copying bytes in window, waiting for space
    icmLit,      // O: got literal, waiting for output space
    icmWash,     // O: got eob, possibly still output waiting
    icmZEnd,     // X: got eob and all data flushed
    icmBadCode   // X: got error
  );

  // inflate codes private state 
  PInflateCodesState = ^TInflateCodesState;
  TInflateCodesState = Record
    Mode: TInflateCodesMode;    // current inflate codes mode
    // mode dependent information
    Len: Cardinal;
    Sub: Record                 // submode
      Case Byte Of
        0:
          (Code: Record         // if Len or Distance, where in tree
             Tree: PInflateHuft; // pointer into tree
             need: Cardinal;    // bits needed
           End);
        1:
          (lit: Cardinal);      // if icmLit, literal
        2:
          (Copy: Record         // if EXT or icmCopy, where and how much
             get: Cardinal;     // bits to get for extra
             Distance: Cardinal; // distance back to copy from
           End);
    End;

    // mode independent information
    LiteralTreeBits: Byte;      // LiteralTree bits decoded per branch
    DistanceTreeBits: Byte;     // DistanceTree bits decoder per branch
    LiteralTree: PInflateHuft;  // literal/length/eob tree
    DistanceTree: PInflateHuft; // distance tree
  End;

  TCheckFunction = Function(Check: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;

  TInflateBlockMode = (
    ibmZType,     // get type bits (3, including end bit)
    ibmLens,      // get lengths for stored
    ibmStored,    // processing stored block
    ibmTable,     // get table lengths
    ibmBitTree,   // get bit lengths tree for a dynamic block
    ibmDistTree,  // get length, distance trees for a dynamic block
    ibmCodes,     // processing fixed or dynamic block
    ibmDry,       // output remaining window bytes
    ibmBlockDone, // finished last block, done
    ibmBlockBad   // got a data error -> stuck here
  );

  // inflate blocks semi-private state
  PInflateBlocksState = ^TInflateBlocksState;
  TInflateBlocksState = Record
    Mode: TInflateBlockMode;     // current inflate block mode
    // mode dependent information 
    Sub: Record                        // submode
      Case Byte Of
        0:
          (left: Cardinal);            // if ibmStored, bytes left to copy
        1:
          (Trees: Record               // if DistanceTree, decoding info for trees
             Table: Cardinal;          // table lengths (14 Bits)
             Index: Cardinal;          // index into blens (or BitOrder)
             blens: PCardinalArray;    // bit lengths of codes
             BB: Cardinal;             // bit length tree depth
             TB: PInflateHuft;         // bit length decoding tree
           End);
        2:
          (decode: Record              // if ibmCodes, current state
             TL: PInflateHuft;
             TD: PInflateHuft;         // trees to free
             codes: PInflateCodesState;
           End);
    End;
    Last: Boolean;                     // True if this block is the last block

    // mode independent information
    bitk: Cardinal;                    // bits in bit buffer
    bitb: Cardinal;                    // bit buffer
    hufts: PHuftField;                 // single allocation for tree space
    window: PByte;                     // sliding window
    zend: PByte;                       // one byte after sliding window
    Read: PByte;                       // window read pointer
    Write: PByte;                      // window write pointer
    CheckFunction: TCheckFunction;     // check function
    Check: Cardinal;                   // check on output
  End;

  TInflateMode = (
    imMethod,   // waiting for imMethod Byte
    imFlag,     // waiting for flag byte
    imDict4,    // four dictionary check bytes to go
    imDict3,    // three dictionary check bytes to go
    imDict2,    // two dictionary check bytes to go
    imDict1,    // one dictionary check byte to go
    imDict0,    // waiting for InflateSetDictionary
    imBlocks,   // decompressing blocks
    imCheck4,   // four check bytes to go
    imCheck3,   // three check bytes to go
    imCheck2,   // two check bytes to go
    imCheck1,   // one check byte to go
    imDone,     // finished check, done
    imBad       // got an error -> stay here
  );

  // inflate private state
  PInternalState = ^TInternalState;
  TInternalState = Record
    Mode: TInflateMode;                // current inflate mode
    // mode dependent information
    Sub: Record                        // submode
      Case Byte Of
        0:
          (imMethod: Cardinal);        // if FLAGS, imMethod byte
        1:
          (Check: Record               // if check, check values to compare
             was: Cardinal;            // computed check value
             need: Cardinal;           // stream check value
           End);
        2:
         (marker: Cardinal);           // if imBad, InflateSync's marker bytes count
    End;

     // mode independent information
    nowrap: Boolean;                   // flag for no wrapper
    wbits: Cardinal;                   // log2(window Size) (8..15, defaults to 15)
    blocks: PInflateBlocksState;       // current InflateBlocks state
  End;


  // The application must update NextInput and AvailableInput when AvailableInput has dropped to zero. It must update
  // NextOutput and AvailableOutput when AvailableOutput has dropped to zero. All other fields are set by the
  // compression library and must not be updated by the application.
  //
  // The fields TotalInput and TotalOutput can be used for statistics or progress reports. After compression, TotalInput
  // holds the total size of the uncompressed data and may be saved for use in the decompressor
  // (particularly if the decompressor wants to decompress everything in a single step).

  PZState = ^TZState;
  TZState = Record
    NextInput: PByte;           // next input byte
    AvailableInput: Cardinal;   // number of bytes available at NextInput
    TotalInput: Cardinal;       // total number of input bytes read so far
    NextOutput: PByte;          // next output byte should be put there
    AvailableOutput: Cardinal;  // remaining free space at NextOutput
    TotalOutput: Cardinal;      // total number of bytes output so far
    Msg: String;                // last error message, '' if no error
    State: PInternalState;      // not visible by applications
    DataType: Integer;          // best guess about the data type: ASCII or binary
    Adler: Cardinal;            // Adler32 value of the uncompressed data
  End;

Const
  // allowed flush values, see Deflate below for details
  Z_NO_FLUSH = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  // Return codes for the compression/decompression functions. Negative
  // values are errors, positive values are used for special but normal events.
  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -1;
  Z_STREAM_ERROR = -2;
  Z_DATA_ERROR = -3;
  Z_MEM_ERROR = -4;
  Z_BUF_ERROR = -5;
  Z_VERSION_ERROR = -6;

  // compression levels
  Z_DEFAULT_COMPRESSION = -1;
  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;

  // compression strategy, see DeflateInit2 below for details 
  Z_DEFAULT_STRATEGY = 0;
  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;

  // possible values of the DataType field
  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;

  // the Deflate compression imMethod (the only one supported in this Version) 
  Z_DEFLATED = 8;

  // three kinds of block type
  STORED_BLOCK = 0;
  STATIC_TREES = 1;
  DYN_TREES = 2;

  // minimum and maximum match lengths
  MIN_MATCH = 3;
  MAX_MATCH = 258;

  // preset dictionary flag in zlib header
  PRESET_DICT = $20;

  ZLIB_VERSION: String[10] = '1.1.2';

  ERROR_BASE = Z_NEED_DICT;
  ErrorMessages: Array[0..9] Of String = (
    SNeedDict,            // Z_NEED_DICT       2
    SStreamEnd,           // Z_STREAM_END      1
    '',                   // Z_OK              0
    SFileError,           // Z_ERRNO          -1
    SStreamError,         // Z_STREAM_ERROR   -2
    SDataError,           // Z_DATA_ERROR     -3
    SInsufficientMemory,  // Z_MEM_ERROR      -4
    SBufferError,         // Z_BUF_ERROR      -5
    SIncompatibleVersion, // Z_VERSION_ERROR  -6
    ''
  );

Function zError(Error: Integer): String;
Function CRC32(CRC: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;

//----------------- deflation support ----------------------------------------------------------------------------------

Function DeflateInit(Var ZState: TZState; Level: Integer): Integer;
Function DeflateInit_(ZState: PZState; Level: Integer; Const Version: String; StreamSize: Integer): Integer;
Function Deflate(Var ZState: TZState; Flush: Integer): Integer;
Function DeflateEnd(Var ZState: TZState): Integer;

// The following functions are needed only in some special applications.
Function DeflateInit2(Var ZState: TZState; Level: Integer; Method: Byte; AWindowBits: Integer; MemLevel: Integer;
  Strategy: Integer): Integer;
Function DeflateSetDictionary(Var ZState: TZState; Dictionary: PByte; DictLength: Cardinal): Integer;
Function DeflateCopy(Dest: PZState; Source: PZState): Integer;
Function DeflateReset(Var ZState: TZState): Integer;
Function DeflateParams(Var ZState: TZState; Level: Integer; Strategy: Integer): Integer;

Const
  LENGTH_CODES = 29;         // number of length codes, not counting the special END_BLOCK code
  LITERALS = 256;            // number of literal bytes 0..255
  L_CODES = (LITERALS + 1 + LENGTH_CODES);
                             // number of literal or length codes, including the END_BLOCK code
  D_CODES = 30;              // number of distance codes
  BL_CODES = 19;             // number of codes used to transfer the bit lengths
  HEAP_SIZE = (2 * L_CODES + 1); // maximum heap size
  MAX_BITS = 15;             // all codes must not exceed MAX_BITS bits

  // stream status 
  INIT_STATE =  42;
  BUSY_STATE =  113;
  FINISH_STATE = 666;

Type
  // data structure describing a single value and its code string 
  PTreeEntry = ^TTreeEntry;
  TTreeEntry = Record
    fc: Record
      Case Byte Of
        0:
          (Frequency: Word); // frequency count
        1:
          (Code: Word); // bit string
    End;
    dl: Record
      Case Byte Of
        0:
          (dad: Word);  // father node in Huffman tree
        1:
          (Len: Word);  // length of bit string
    End;
  End;

  TLiteralTree = Array[0..HEAP_SIZE - 1] Of TTreeEntry; // literal and length tree
  TDistanceTree = Array[0..2 * D_CODES] Of TTreeEntry; // distance tree
  THuffmanTree = Array[0..2 * BL_CODES] Of TTreeEntry; // Huffman tree for bit lengths

  PTree = ^TTree;
  TTree = Array[0..(MaxInt Div SizeOf(TTreeEntry)) - 1] Of TTreeEntry; // generic tree type

  PStaticTreeDescriptor = ^TStaticTreeDescriptor;
  TStaticTreeDescriptor = Record
    StaticTree: PTree;        // static tree or nil
    ExtraBits: PIntegerArray; // extra bits for each code or nil
    ExtraBase: Integer;       // base index for ExtraBits
    Elements: Integer;        // max number of elements in the tree
    MaxLength: Integer;       // max bit length for the codes
  End;
  
  PTreeDescriptor = ^TTreeDescriptor;
  TTreeDescriptor = Record
    DynamicTree: PTree;     
    MaxCode: Integer;                        // largest code with non zero frequency
    StaticDescriptor: PStaticTreeDescriptor; // the corresponding static tree
  End;

  PDeflateState = ^TDeflateState;
  TDeflateState = Record
    ZState: PZState;            // pointer back to this zlib stream
    Status: Integer;            // as the name implies
    PendingBuffer: PByteArray;  // output still pending
    PendingBufferSize: Integer;
    PendingOutput: PByte;       // next pending byte to output to the stream
    Pending: Integer;           // nb of bytes in the pending buffer
    NoHeader: Integer;          // suppress zlib header and Adler32
    DataType: Byte;             // UNKNOWN, BINARY or ASCII
    imMethod: Byte;             // ibmStored (for zip only) or DEFLATED
    LastFlush: Integer;         // Value of flush param for previous deflate call
    WindowSize: Cardinal;       // LZ77 window size (32K by default)
    WindowBits: Cardinal;       // log2(WindowSize) (8..16)
    WindowMask: Cardinal;       // WindowSize - 1

    // Sliding window. Input bytes are read into the second half of the window,
    // and move to the first half later to keep a dictionary of at least WSize
    // bytes. With this organization, matches are limited to a distance of
    // WSize - MAX_MATCH bytes, but this ensures that IO is always
    // performed with a length multiple of the block Size. Also, it limits
    // the window Size to 64K, which is quite useful on MSDOS.
    // To do: use the user input buffer as sliding window.
    Window: PByteArray;

    // Actual size of Window: 2 * WSize, except when the user input buffer
    // is directly used as sliding window.
    CurrentWindowSize: Integer;

    // Link to older string with same hash index. to limit the size of this
    // array to 64K, this link is maintained only for the last 32K strings.
    // An index in this array is thus a window index modulo 32K.
    Previous: PWordArray;

    Head: PWordArray;           // heads of the hash chains or nil

    InsertHash: Cardinal;       // hash index of string to be inserted
    HashSize: Cardinal;         // number of elements in hash table
    HashBits: Cardinal;         // log2(HashSize)
    HashMask: Cardinal;         // HashSize - 1

    // Number of bits by which InsertHash must be shifted at each input step.
    // It must be such that after MIN_MATCH steps, the oldest byte no longer
    // takes part in the hash key, that is:
    // HashShift * MIN_MATCH >= HashBits
    HashShift: Cardinal;

    // Window position at the beginning of the current output block. Gets
    // negative when the window is moved backwards. 
    BlockStart: Integer;

    MatchLength: Cardinal;      // length of best match
    PreviousMatch: Cardinal;    // previous match
    MatchAvailable: Boolean;    // set if previous match exists
    StringStart: Cardinal;      // start of string to insert
    MatchStart: Cardinal;       // start of matching string
    Lookahead: Cardinal;        // number of valid bytes ahead in window 

    // Length of the best match at previous step. Matches not greater than this
    // are discarded. This is used in the lazy match evaluation.
    PreviousLength: Cardinal;

    // To speed up deflation hash chains are never searched beyond this
    // Length. A higher limit improves compression ratio but degrades the speed. 
    MaxChainLength: Cardinal;

    Level: Integer;             // compression level (1..9)
    Strategy: Integer;          // favor or force Huffman coding
    GoodMatch: Cardinal;        // use a faster search when the previous match is longer than this
    NiceMatch: Cardinal;        // stop searching when current match exceeds this

    LiteralTree: TLiteralTree;  // literal and length tree
    DistanceTree: TDistanceTree; // distance tree
    BitLengthTree: THuffmanTree; // Huffman tree for bit lengths

    LiteralDescriptor: TTreeDescriptor; // Descriptor for literal tree
    DistanceDescriptor: TTreeDescriptor; // Descriptor for distance tree
    BitLengthDescriptor: TTreeDescriptor; // Descriptor for bit length tree 

    BitLengthCounts: Array[0..MAX_BITS] Of Word; // number of codes at each bit length for an optimal tree

    Heap: Array[0..2 * L_CODES] Of Integer; // heap used to build the Huffman trees 
    HeapLength: Integer;        // number of elements in the heap
    HeapMaximum: Integer;       // element of largest frequency
    // The sons of Heap[N] are Heap[2 * N] and Heap[2 * N + 1]. Heap[0] is not used.
    // The same heap array is used to build all trees.

    Depth: Array[0..2 * L_CODES] Of Byte; // depth of each subtree used as tie breaker for trees of equal frequency

    LiteralBuffer: PByteArray;       // buffer for literals or lengths

    // Size of match buffer for literals/lengths. There are 4 reasons for limiting LiteralBufferSize to 64K:
    //  - frequencies can be kept in 16 bit counters
    //  - If compression is not successful for the first block, all input
    //    data is still in the window so we can still emit a stored block even
    //    when input comes from standard input. This can also be done for
    //    all blocks if LiteralBufferSize is not greater than 32K.
    //  - if compression is not successful for a file smaller than 64K, we can
    //    even emit a stored file instead of a stored block (saving 5 bytes).
    //    This is applicable only for zip (not gzip or zlib).
    //  - creating new Huffman trees less frequently may not provide fast
    //    adaptation to changes in the input data statistics. (Take for
    //    example a binary file with poorly compressible code followed by
    //    a highly compressible string table.) Smaller buffer sizes give
    //    fast adaptation but have of course the overhead of transmitting
    //    trees more frequently.
    //  - I can't count above 4
    LiteralBufferSize: Cardinal;

    LastLiteral: Cardinal;      // running index in LiteralBuffer

    // Buffer for distances. To simplify the code, DistanceBuffer and LiteralBuffer have
    // the same number of elements. To use different lengths, an extra flag array would be necessary.
    DistanceBuffer: PWordArray;

    OptimalLength: Integer;     // bit length of current block with optimal trees
    StaticLength: Integer;      // bit length of current block with static trees
    CompressedLength: Integer;  // total bit length of compressed file
    Matches: Cardinal;          // number of string matches in current block
    LastEOBLength: Integer;     // bit length of EOB code for last block
    BitsBuffer: Word;           // Output buffer. Bits are inserted starting at the bottom (least significant bits).
    ValidBits: Integer;         // Number of valid bits in BitsBuffer. All Bits above the last valid bit are always zero.
    Case Byte Of
      0:
        // Attempt to find a better match only when the current match is strictly smaller than this value.
        // This mechanism is used only for compression levels >= 4.
        (MaxLazyMatch: Cardinal);
      1:
        // Insert new strings in the hash table only if the match Length is not greater than this length. This saves
        // time but degrades compression. MaxInsertLength is used only for compression levels <= 3. 
        (MaxInsertLength: Cardinal);
  End;

//----------------- inflation support ----------------------------------------------------------------------------------

Function InflateInit(Var Z: TZState): Integer;
Function InflateInit_(Var Z: TZState; Const Version: String; StreamSize: Integer): Integer;
Function InflateInit2_(Var Z: TZState; W: Integer; Const Version: AnsiString; StreamSize: Integer): Integer;
Function InflateInit2(Var Z: TZState; AWindowBits: Integer): Integer;
Function InflateEnd(Var Z: TZState): Integer;
Function InflateReset(Var Z: TZState): Integer;
Function Inflate(Var Z: TZState; F: Integer): Integer;
Function InflateSetDictionary(Var Z: TZState; Dictionary: PByte; DictLength: Cardinal): Integer;
Function InflateSync(Var Z: TZState): Integer;
Function IsInflateSyncPoint(Var Z: TZState): Integer;

//----------------------------------------------------------------------------------------------------------------------

Implementation

Uses
  SysUtils;

Const
  // Adler checksum
  Base = Cardinal(65521);       // largest prime smaller than 65536 
  NMAX = 3854;                  // Code with signed 32 bit integer
  
Type
  LH = Record
    L, H: Word;
  End;

//----------------------------------------------------------------------------------------------------------------------

Function zError(Error: Integer): String;

Begin
  Result := ErrorMessages[Z_NEED_DICT - Error];
End;

//----------------------------------------------------------------------------------------------------------------------

Function Adler32(Adler: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;

Var
  s1, s2: Cardinal;
  K: Integer;

Begin
  s1 := Adler And $FFFF;
  s2 := (Adler Shr 16) And $FFFF;

  If Buffer = Nil Then Result := 1
                  Else
  Begin                                             
    While Len > 0 Do
    Begin
      If Len < NMAX Then K := Len
                    Else K := NMAX;
      Dec(Len, K);
      While K > 0 Do
      Begin
        Inc(s1, Buffer^);
        Inc(s2, s1);                      
        Inc(Buffer);
        Dec(K);
      End;
      s1 := s1 Mod Base;
      s2 := s2 Mod Base;
    End;
    Result := (s2 Shl 16) Or s1;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Var
  // used to calculate the running CRC of a bunch of bytes,
  // this table is dynamically created in order to save space if never needed
  CRCTable: Array Of Cardinal;

Procedure MakeCRCTable;

// creates the CRC table when it is needed the first time

Var
  C: Cardinal;
  N, K : Integer;
  Poly: Cardinal; // polynomial exclusive-or pattern

Const
 // terms of polynomial defining this CRC (except x^32)
 P: Array [0..13] Of Byte = (0, 1, 2, 4, 5, 7, 8, 10, 11, 12, 16, 22, 23, 26);

Begin
  // make exclusive-or pattern from polynomial ($EDB88320)
  SetLength(CRCTable, 256);
  Poly := 0;
  For N := 0 To SizeOf(P) - 1 Do
    Poly := Poly Or (1 Shl (31 - P[N]));

  For N := 0 To 255 Do
  Begin
    C := N;
    For K := 0 To 7 Do
    Begin
      If (C And 1) <> 0 Then C := Poly Xor (C Shr 1)
                        Else C := C Shr 1;
    End;
    CRCTable[N] := C;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function CRC32(CRC: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;

// Generate a table for a byte-wise 32-bit CRC calculation on the polynomial:
// x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1.
//
// Polynomials over GF(2) are represented in binary, one bit per coefficient,
// with the lowest powers in the most significant bit.  Then adding polynomials
// is just exclusive-or, and multiplying a polynomial by x is a right shift by
// one.  If we call the above polynomial p, and represent a byte as the
// polynomial q, also with the lowest power in the most significant bit (so the
// byte 0xb1 is the polynomial x^7+x^3+x+1), then the CRC is (q*x^32) mod p,
// where a mod b means the remainder after dividing a by b.
//
// This calculation is done using the shift-register method of multiplying and
// taking the remainder.  The register is initialized to zero, and for each
// incoming bit, x^32 is added mod p to the register if the bit is a one (where
// x^32 mod p is p+x^32 = x^26+...+1), and the register is multiplied mod p by
// x (which is shifting right by one and adding x^32 mod p if the bit shifted
// out is a one).  We start with the highest power (least significant bit) of
// q and repeat for all eight bits of q.
//
// The table is simply the CRC of all possible eight bit values.  This is all
// the information needed to generate CRC's on data a byte at a time for all
// combinations of CRC register values and incoming bytes.

Begin
  If Buffer = Nil Then Result := 0
                  Else
  Begin
    If CRCTable = Nil Then MakeCRCTable;

    CRC := CRC Xor $FFFFFFFF;
    While Len >= 8 Do
    Begin
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) Xor Buffer^] Xor (CRC Shr 8);
      Inc(Buffer);

      Dec(Len, 8);
    End;

    While Len > 0 Do
    Begin
      CRC := CRCTable[(CRC Xor Buffer^) And $FF] Xor (CRC Shr 8);
      Inc(Buffer);
      Dec(Len);
    End;
    Result := CRC Xor $FFFFFFFF;
  End;
End;

//----------------- Huffmann trees -------------------------------------------------------------------------------------

Const
  DIST_CODE_LEN = 512; // see definition of array dist_code below

  // The static literal tree. Since the bit lengths are imposed, there is no need for the L_CODES Extra codes used
  // during heap construction. However the codes 286 and 287 are needed to build a canonical tree (see TreeInit below).
  StaticLiteralTree: Array[0..L_CODES + 1] Of TTreeEntry = (
    (fc: (Frequency:  12); dl: (Len: 8)), (fc: (Frequency: 140); dl: (Len: 8)), (fc: (Frequency:  76); dl: (Len: 8)),
    (fc: (Frequency: 204); dl: (Len: 8)), (fc: (Frequency:  44); dl: (Len: 8)), (fc: (Frequency: 172); dl: (Len: 8)),
    (fc: (Frequency: 108); dl: (Len: 8)), (fc: (Frequency: 236); dl: (Len: 8)), (fc: (Frequency:  28); dl: (Len: 8)),
    (fc: (Frequency: 156); dl: (Len: 8)), (fc: (Frequency:  92); dl: (Len: 8)), (fc: (Frequency: 220); dl: (Len: 8)),
    (fc: (Frequency:  60); dl: (Len: 8)), (fc: (Frequency: 188); dl: (Len: 8)), (fc: (Frequency: 124); dl: (Len: 8)),
    (fc: (Frequency: 252); dl: (Len: 8)), (fc: (Frequency:   2); dl: (Len: 8)), (fc: (Frequency: 130); dl: (Len: 8)),
    (fc: (Frequency:  66); dl: (Len: 8)), (fc: (Frequency: 194); dl: (Len: 8)), (fc: (Frequency:  34); dl: (Len: 8)),
    (fc: (Frequency: 162); dl: (Len: 8)), (fc: (Frequency:  98); dl: (Len: 8)), (fc: (Frequency: 226); dl: (Len: 8)),
    (fc: (Frequency:  18); dl: (Len: 8)), (fc: (Frequency: 146); dl: (Len: 8)), (fc: (Frequency:  82); dl: (Len: 8)),
    (fc: (Frequency: 210); dl: (Len: 8)), (fc: (Frequency:  50); dl: (Len: 8)), (fc: (Frequency: 178); dl: (Len: 8)),
    (fc: (Frequency: 114); dl: (Len: 8)), (fc: (Frequency: 242); dl: (Len: 8)), (fc: (Frequency:  10); dl: (Len: 8)),
    (fc: (Frequency: 138); dl: (Len: 8)), (fc: (Frequency:  74); dl: (Len: 8)), (fc: (Frequency: 202); dl: (Len: 8)),
    (fc: (Frequency:  42); dl: (Len: 8)), (fc: (Frequency: 170); dl: (Len: 8)), (fc: (Frequency: 106); dl: (Len: 8)),
    (fc: (Frequency: 234); dl: (Len: 8)), (fc: (Frequency:  26); dl: (Len: 8)), (fc: (Frequency: 154); dl: (Len: 8)),
    (fc: (Frequency:  90); dl: (Len: 8)), (fc: (Frequency: 218); dl: (Len: 8)), (fc: (Frequency:  58); dl: (Len: 8)),
    (fc: (Frequency: 186); dl: (Len: 8)), (fc: (Frequency: 122); dl: (Len: 8)), (fc: (Frequency: 250); dl: (Len: 8)),
    (fc: (Frequency:   6); dl: (Len: 8)), (fc: (Frequency: 134); dl: (Len: 8)), (fc: (Frequency:  70); dl: (Len: 8)),
    (fc: (Frequency: 198); dl: (Len: 8)), (fc: (Frequency:  38); dl: (Len: 8)), (fc: (Frequency: 166); dl: (Len: 8)),
    (fc: (Frequency: 102); dl: (Len: 8)), (fc: (Frequency: 230); dl: (Len: 8)), (fc: (Frequency:  22); dl: (Len: 8)),
    (fc: (Frequency: 150); dl: (Len: 8)), (fc: (Frequency:  86); dl: (Len: 8)), (fc: (Frequency: 214); dl: (Len: 8)),
    (fc: (Frequency:  54); dl: (Len: 8)), (fc: (Frequency: 182); dl: (Len: 8)), (fc: (Frequency: 118); dl: (Len: 8)),
    (fc: (Frequency: 246); dl: (Len: 8)), (fc: (Frequency:  14); dl: (Len: 8)), (fc: (Frequency: 142); dl: (Len: 8)),
    (fc: (Frequency:  78); dl: (Len: 8)), (fc: (Frequency: 206); dl: (Len: 8)), (fc: (Frequency:  46); dl: (Len: 8)),
    (fc: (Frequency: 174); dl: (Len: 8)), (fc: (Frequency: 110); dl: (Len: 8)), (fc: (Frequency: 238); dl: (Len: 8)),
    (fc: (Frequency:  30); dl: (Len: 8)), (fc: (Frequency: 158); dl: (Len: 8)), (fc: (Frequency:  94); dl: (Len: 8)),
    (fc: (Frequency: 222); dl: (Len: 8)), (fc: (Frequency:  62); dl: (Len: 8)), (fc: (Frequency: 190); dl: (Len: 8)),
    (fc: (Frequency: 126); dl: (Len: 8)), (fc: (Frequency: 254); dl: (Len: 8)), (fc: (Frequency:   1); dl: (Len: 8)),
    (fc: (Frequency: 129); dl: (Len: 8)), (fc: (Frequency:  65); dl: (Len: 8)), (fc: (Frequency: 193); dl: (Len: 8)),
    (fc: (Frequency:  33); dl: (Len: 8)), (fc: (Frequency: 161); dl: (Len: 8)), (fc: (Frequency:  97); dl: (Len: 8)),
    (fc: (Frequency: 225); dl: (Len: 8)), (fc: (Frequency:  17); dl: (Len: 8)), (fc: (Frequency: 145); dl: (Len: 8)),
    (fc: (Frequency:  81); dl: (Len: 8)), (fc: (Frequency: 209); dl: (Len: 8)), (fc: (Frequency:  49); dl: (Len: 8)),
    (fc: (Frequency: 177); dl: (Len: 8)), (fc: (Frequency: 113); dl: (Len: 8)), (fc: (Frequency: 241); dl: (Len: 8)),
    (fc: (Frequency:   9); dl: (Len: 8)), (fc: (Frequency: 137); dl: (Len: 8)), (fc: (Frequency:  73); dl: (Len: 8)),
    (fc: (Frequency: 201); dl: (Len: 8)), (fc: (Frequency:  41); dl: (Len: 8)), (fc: (Frequency: 169); dl: (Len: 8)),
    (fc: (Frequency: 105); dl: (Len: 8)), (fc: (Frequency: 233); dl: (Len: 8)), (fc: (Frequency:  25); dl: (Len: 8)),
    (fc: (Frequency: 153); dl: (Len: 8)), (fc: (Frequency:  89); dl: (Len: 8)), (fc: (Frequency: 217); dl: (Len: 8)),
    (fc: (Frequency:  57); dl: (Len: 8)), (fc: (Frequency: 185); dl: (Len: 8)), (fc: (Frequency: 121); dl: (Len: 8)),
    (fc: (Frequency: 249); dl: (Len: 8)), (fc: (Frequency:   5); dl: (Len: 8)), (fc: (Frequency: 133); dl: (Len: 8)),
    (fc: (Frequency:  69); dl: (Len: 8)), (fc: (Frequency: 197); dl: (Len: 8)), (fc: (Frequency:  37); dl: (Len: 8)),
    (fc: (Frequency: 165); dl: (Len: 8)), (fc: (Frequency: 101); dl: (Len: 8)), (fc: (Frequency: 229); dl: (Len: 8)),
    (fc: (Frequency:  21); dl: (Len: 8)), (fc: (Frequency: 149); dl: (Len: 8)), (fc: (Frequency:  85); dl: (Len: 8)),
    (fc: (Frequency: 213); dl: (Len: 8)), (fc: (Frequency:  53); dl: (Len: 8)), (fc: (Frequency: 181); dl: (Len: 8)),
    (fc: (Frequency: 117); dl: (Len: 8)), (fc: (Frequency: 245); dl: (Len: 8)), (fc: (Frequency:  13); dl: (Len: 8)),
    (fc: (Frequency: 141); dl: (Len: 8)), (fc: (Frequency:  77); dl: (Len: 8)), (fc: (Frequency: 205); dl: (Len: 8)),
    (fc: (Frequency:  45); dl: (Len: 8)), (fc: (Frequency: 173); dl: (Len: 8)), (fc: (Frequency: 109); dl: (Len: 8)),
    (fc: (Frequency: 237); dl: (Len: 8)), (fc: (Frequency:  29); dl: (Len: 8)), (fc: (Frequency: 157); dl: (Len: 8)),
    (fc: (Frequency:  93); dl: (Len: 8)), (fc: (Frequency: 221); dl: (Len: 8)), (fc: (Frequency:  61); dl: (Len: 8)),
    (fc: (Frequency: 189); dl: (Len: 8)), (fc: (Frequency: 125); dl: (Len: 8)), (fc: (Frequency: 253); dl: (Len: 8)),
    (fc: (Frequency:  19); dl: (Len: 9)), (fc: (Frequency: 275); dl: (Len: 9)), (fc: (Frequency: 147); dl: (Len: 9)),
    (fc: (Frequency: 403); dl: (Len: 9)), (fc: (Frequency:  83); dl: (Len: 9)), (fc: (Frequency: 339); dl: (Len: 9)),
    (fc: (Frequency: 211); dl: (Len: 9)), (fc: (Frequency: 467); dl: (Len: 9)), (fc: (Frequency:  51); dl: (Len: 9)),
    (fc: (Frequency: 307); dl: (Len: 9)), (fc: (Frequency: 179); dl: (Len: 9)), (fc: (Frequency: 435); dl: (Len: 9)),
    (fc: (Frequency: 115); dl: (Len: 9)), (fc: (Frequency: 371); dl: (Len: 9)), (fc: (Frequency: 243); dl: (Len: 9)),
    (fc: (Frequency: 499); dl: (Len: 9)), (fc: (Frequency:  11); dl: (Len: 9)), (fc: (Frequency: 267); dl: (Len: 9)),
    (fc: (Frequency: 139); dl: (Len: 9)), (fc: (Frequency: 395); dl: (Len: 9)), (fc: (Frequency:  75); dl: (Len: 9)),
    (fc: (Frequency: 331); dl: (Len: 9)), (fc: (Frequency: 203); dl: (Len: 9)), (fc: (Frequency: 459); dl: (Len: 9)),
    (fc: (Frequency:  43); dl: (Len: 9)), (fc: (Frequency: 299); dl: (Len: 9)), (fc: (Frequency: 171); dl: (Len: 9)),
    (fc: (Frequency: 427); dl: (Len: 9)), (fc: (Frequency: 107); dl: (Len: 9)), (fc: (Frequency: 363); dl: (Len: 9)),
    (fc: (Frequency: 235); dl: (Len: 9)), (fc: (Frequency: 491); dl: (Len: 9)), (fc: (Frequency:  27); dl: (Len: 9)),
    (fc: (Frequency: 283); dl: (Len: 9)), (fc: (Frequency: 155); dl: (Len: 9)), (fc: (Frequency: 411); dl: (Len: 9)),
    (fc: (Frequency:  91); dl: (Len: 9)), (fc: (Frequency: 347); dl: (Len: 9)), (fc: (Frequency: 219); dl: (Len: 9)),
    (fc: (Frequency: 475); dl: (Len: 9)), (fc: (Frequency:  59); dl: (Len: 9)), (fc: (Frequency: 315); dl: (Len: 9)),
    (fc: (Frequency: 187); dl: (Len: 9)), (fc: (Frequency: 443); dl: (Len: 9)), (fc: (Frequency: 123); dl: (Len: 9)),
    (fc: (Frequency: 379); dl: (Len: 9)), (fc: (Frequency: 251); dl: (Len: 9)), (fc: (Frequency: 507); dl: (Len: 9)),
    (fc: (Frequency:   7); dl: (Len: 9)), (fc: (Frequency: 263); dl: (Len: 9)), (fc: (Frequency: 135); dl: (Len: 9)),
    (fc: (Frequency: 391); dl: (Len: 9)), (fc: (Frequency:  71); dl: (Len: 9)), (fc: (Frequency: 327); dl: (Len: 9)),
    (fc: (Frequency: 199); dl: (Len: 9)), (fc: (Frequency: 455); dl: (Len: 9)), (fc: (Frequency:  39); dl: (Len: 9)),
    (fc: (Frequency: 295); dl: (Len: 9)), (fc: (Frequency: 167); dl: (Len: 9)), (fc: (Frequency: 423); dl: (Len: 9)),
    (fc: (Frequency: 103); dl: (Len: 9)), (fc: (Frequency: 359); dl: (Len: 9)), (fc: (Frequency: 231); dl: (Len: 9)),
    (fc: (Frequency: 487); dl: (Len: 9)), (fc: (Frequency:  23); dl: (Len: 9)), (fc: (Frequency: 279); dl: (Len: 9)),
    (fc: (Frequency: 151); dl: (Len: 9)), (fc: (Frequency: 407); dl: (Len: 9)), (fc: (Frequency:  87); dl: (Len: 9)),
    (fc: (Frequency: 343); dl: (Len: 9)), (fc: (Frequency: 215); dl: (Len: 9)), (fc: (Frequency: 471); dl: (Len: 9)),
    (fc: (Frequency:  55); dl: (Len: 9)), (fc: (Frequency: 311); dl: (Len: 9)), (fc: (Frequency: 183); dl: (Len: 9)),
    (fc: (Frequency: 439); dl: (Len: 9)), (fc: (Frequency: 119); dl: (Len: 9)), (fc: (Frequency: 375); dl: (Len: 9)),
    (fc: (Frequency: 247); dl: (Len: 9)), (fc: (Frequency: 503); dl: (Len: 9)), (fc: (Frequency:  15); dl: (Len: 9)),
    (fc: (Frequency: 271); dl: (Len: 9)), (fc: (Frequency: 143); dl: (Len: 9)), (fc: (Frequency: 399); dl: (Len: 9)),
    (fc: (Frequency:  79); dl: (Len: 9)), (fc: (Frequency: 335); dl: (Len: 9)), (fc: (Frequency: 207); dl: (Len: 9)),
    (fc: (Frequency: 463); dl: (Len: 9)), (fc: (Frequency:  47); dl: (Len: 9)), (fc: (Frequency: 303); dl: (Len: 9)),
    (fc: (Frequency: 175); dl: (Len: 9)), (fc: (Frequency: 431); dl: (Len: 9)), (fc: (Frequency: 111); dl: (Len: 9)),
    (fc: (Frequency: 367); dl: (Len: 9)), (fc: (Frequency: 239); dl: (Len: 9)), (fc: (Frequency: 495); dl: (Len: 9)),
    (fc: (Frequency:  31); dl: (Len: 9)), (fc: (Frequency: 287); dl: (Len: 9)), (fc: (Frequency: 159); dl: (Len: 9)),
    (fc: (Frequency: 415); dl: (Len: 9)), (fc: (Frequency:  95); dl: (Len: 9)), (fc: (Frequency: 351); dl: (Len: 9)),
    (fc: (Frequency: 223); dl: (Len: 9)), (fc: (Frequency: 479); dl: (Len: 9)), (fc: (Frequency:  63); dl: (Len: 9)),
    (fc: (Frequency: 319); dl: (Len: 9)), (fc: (Frequency: 191); dl: (Len: 9)), (fc: (Frequency: 447); dl: (Len: 9)),
    (fc: (Frequency: 127); dl: (Len: 9)), (fc: (Frequency: 383); dl: (Len: 9)), (fc: (Frequency: 255); dl: (Len: 9)),
    (fc: (Frequency: 511); dl: (Len: 9)), (fc: (Frequency:   0); dl: (Len: 7)), (fc: (Frequency:  64); dl: (Len: 7)),
    (fc: (Frequency:  32); dl: (Len: 7)), (fc: (Frequency:  96); dl: (Len: 7)), (fc: (Frequency:  16); dl: (Len: 7)),
    (fc: (Frequency:  80); dl: (Len: 7)), (fc: (Frequency:  48); dl: (Len: 7)), (fc: (Frequency: 112); dl: (Len: 7)),
    (fc: (Frequency:   8); dl: (Len: 7)), (fc: (Frequency:  72); dl: (Len: 7)), (fc: (Frequency:  40); dl: (Len: 7)),
    (fc: (Frequency: 104); dl: (Len: 7)), (fc: (Frequency:  24); dl: (Len: 7)), (fc: (Frequency:  88); dl: (Len: 7)),
    (fc: (Frequency:  56); dl: (Len: 7)), (fc: (Frequency: 120); dl: (Len: 7)), (fc: (Frequency:   4); dl: (Len: 7)),
    (fc: (Frequency:  68); dl: (Len: 7)), (fc: (Frequency:  36); dl: (Len: 7)), (fc: (Frequency: 100); dl: (Len: 7)),
    (fc: (Frequency:  20); dl: (Len: 7)), (fc: (Frequency:  84); dl: (Len: 7)), (fc: (Frequency:  52); dl: (Len: 7)),
    (fc: (Frequency: 116); dl: (Len: 7)), (fc: (Frequency:   3); dl: (Len: 8)), (fc: (Frequency: 131); dl: (Len: 8)),
    (fc: (Frequency:  67); dl: (Len: 8)), (fc: (Frequency: 195); dl: (Len: 8)), (fc: (Frequency:  35); dl: (Len: 8)),
    (fc: (Frequency: 163); dl: (Len: 8)), (fc: (Frequency:  99); dl: (Len: 8)), (fc: (Frequency: 227); dl: (Len: 8))
  );

  // The static distance tree. (Actually a trivial tree since all lens use 5 Bits.)
  StaticDescriptorTree: Array[0..D_CODES - 1] Of TTreeEntry = (
    (fc: (Frequency:  0); dl: (Len: 5)), (fc: (Frequency: 16); dl: (Len: 5)), (fc: (Frequency:  8); dl: (Len: 5)),
    (fc: (Frequency: 24); dl: (Len: 5)), (fc: (Frequency:  4); dl: (Len: 5)), (fc: (Frequency: 20); dl: (Len: 5)),
    (fc: (Frequency: 12); dl: (Len: 5)), (fc: (Frequency: 28); dl: (Len: 5)), (fc: (Frequency:  2); dl: (Len: 5)),
    (fc: (Frequency: 18); dl: (Len: 5)), (fc: (Frequency: 10); dl: (Len: 5)), (fc: (Frequency: 26); dl: (Len: 5)),
    (fc: (Frequency:  6); dl: (Len: 5)), (fc: (Frequency: 22); dl: (Len: 5)), (fc: (Frequency: 14); dl: (Len: 5)),
    (fc: (Frequency: 30); dl: (Len: 5)), (fc: (Frequency:  1); dl: (Len: 5)), (fc: (Frequency: 17); dl: (Len: 5)),
    (fc: (Frequency:  9); dl: (Len: 5)), (fc: (Frequency: 25); dl: (Len: 5)), (fc: (Frequency:  5); dl: (Len: 5)),
    (fc: (Frequency: 21); dl: (Len: 5)), (fc: (Frequency: 13); dl: (Len: 5)), (fc: (Frequency: 29); dl: (Len: 5)),
    (fc: (Frequency:  3); dl: (Len: 5)), (fc: (Frequency: 19); dl: (Len: 5)), (fc: (Frequency: 11); dl: (Len: 5)),
    (fc: (Frequency: 27); dl: (Len: 5)), (fc: (Frequency:  7); dl: (Len: 5)), (fc: (Frequency: 23); dl: (Len: 5))
  );

  // Distance codes. The first 256 values correspond to the distances 3 .. 258, the last 256 values correspond to the
  // top 8 Bits of the 15 bit distances.
  DistanceCode: Array[0..DIST_CODE_LEN - 1] Of Byte = (
     0,  1,  2,  3,  4,  4,  5,  5,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,
     8,  8,  8,  8,  9,  9,  9,  9,  9,  9,  9,  9, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,  0,  0, 16, 17,
    18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22,
    23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29
  );

  // length code for each normalized match length (0 = MIN_MATCH)
  LengthCode: Array[0..MAX_MATCH - MIN_MATCH] Of Byte = (
     0,  1,  2,  3,  4,  5,  6,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 12, 12,
    13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16,
    17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19,
    19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28
  );

  // first normalized length for each code (0 = MIN_MATCH) 
  BaseLength: Array[0..LENGTH_CODES - 1] Of Integer = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56,
    64, 80, 96, 112, 128, 160, 192, 224, 0
  );

  // first normalized distance for each code (0 = distance of 1)
  BaseDistance: Array[0..D_CODES - 1] Of Integer = (
       0,     1,     2,     3,     4,     6,     8,    12,    16,    24,
      32,    48,    64,    96,   128,   192,   256,   384,   512,   768,
    1024,  1536,  2048,  3072,  4096,  6144,  8192, 12288, 16384, 24576
  );

  MIN_LOOKAHEAD = (MAX_MATCH + MIN_MATCH + 1);
  MAX_BL_BITS = 7;              // bit length codes must not exceed MAX_BL_BITS bits
  END_BLOCK = 256;              // end of block literal code
  REP_3_6 = 16;                 // repeat previous bit length 3-6 times (2 Bits of repeat count)
  REPZ_3_10 = 17;               // repeat a zero length 3-10 times  (3 Bits of repeat count)
  REPZ_11_138 = 18;             // repeat a zero length 11-138 times  (7 Bits of repeat count)

  // extra bits for each length code 
  ExtraLengthBits: Array[0..LENGTH_CODES - 1] Of Integer = (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0
  );

  // extra bits for each distance code 
  ExtraDistanceBits: Array[0..D_CODES-1] Of Integer = (
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10 ,10, 11, 11, 12, 12, 13, 13
  );

  // extra bits for each bit length code
  ExtraBitLengthBits: Array[0..BL_CODES - 1] Of Integer = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 7
  );

  // The lengths of the bit length codes are sent in order of decreasing probability,
  // to avoid transmitting the lengths for unused bit length codes.
  BitLengthOrder: Array[0..BL_CODES - 1] Of Byte = (
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
  );

  // Number of bits used within BitsBuffer. (BitsBuffer might be implemented on more than 16 bits on some systems.)
  BufferSize = 16;

  StaticLiteralDescriptor: TStaticTreeDescriptor = (
    StaticTree: @StaticLiteralTree;  // pointer to array of TTreeEntry
    ExtraBits: @ExtraLengthBits;     // pointer to array of integer
    ExtraBase: LITERALS + 1;
    Elements: L_CODES;
    MaxLength: MAX_BITS
  );

  StaticDistanceDescriptor: TStaticTreeDescriptor = (
    StaticTree: @StaticDescriptorTree;
    ExtraBits: @ExtraDistanceBits;
    ExtraBase: 0;
    Elements: D_CODES;
    MaxLength: MAX_BITS
  );

  StaticBitLengthDescriptor: TStaticTreeDescriptor = (
    StaticTree: Nil;
    ExtraBits: @ExtraBitLengthBits;
    ExtraBase: 0;
    Elements: BL_CODES;
    MaxLength: MAX_BL_BITS
  );

  SMALLEST = 1; // index within the heap array of least frequent node in the Huffman tree

//----------------------------------------------------------------------------------------------------------------------

Procedure SendBits(Var S: TDeflateState; Value: Word; Length: Integer);

// Value contains what is to be sent
// Length is the number of bits to send

Begin
  // If there's not enough room in BitsBuffer use (valid) bits from BitsBuffer and
  // (16 - ValidBits) bits from Value, leaving (width - (16 - ValidBits)) unused bits in Value.
  {$IFOPT Q+} {$Q-} {$DEFINE OverflowCheck} {$ENDIF}
  {$IFOPT R+} {$R-} {$DEFINE RangeCheck} {$ENDIF}
  If (S.ValidBits > Integer(BufferSize) - Length) Then
  Begin
    S.BitsBuffer := S.BitsBuffer Or (Value Shl S.ValidBits);
    S.PendingBuffer[S.Pending] := S.BitsBuffer And $FF;
    Inc(S.Pending);
    S.PendingBuffer[S.Pending] := S.BitsBuffer Shr 8;
    Inc(S.Pending);

    S.BitsBuffer := Value Shr (BufferSize - S.ValidBits);
    Inc(S.ValidBits, Length - BufferSize);
  End
  Else
  Begin
    S.BitsBuffer := S.BitsBuffer Or (Value Shl S.ValidBits);
    Inc(S.ValidBits, Length);
  End;
  {$IFDEF OverflowCheck} {$Q+} {$UNDEF OverflowCheck} {$ENDIF}
  {$IFDEF RangeCheck} {$R+} {$UNDEF RangeCheck} {$ENDIF}
End;

//----------------------------------------------------------------------------------------------------------------------

Function BitReverse(Code: Word; Len: Integer): Word;

// Reverses the first Len bits of Code, using straightforward code (a faster
// imMethod would use a table)

Begin
  Result := 0;
  Repeat
    Result := Result Or (Code And 1);
    Code := Code Shr 1;
    Result := Result Shl 1;
    Dec(Len);
  Until Len <= 0;
  Result := Result Shr 1;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure GenerateCodes(Tree: PTree; MaxCode: Integer; Const BitLengthCounts: Array Of Word);

// Generates the codes for a given tree and bit counts (which need not be optimal).
// The array BitLengthCounts contains the bit length statistics for the given tree and the field Len is set for all
// Tree elements. MaxCode is the largest code with non zero frequency and BitLengthCounts are the number of codes at
// each bit length.
// On exit the field code is set for all tree elements of non zero code length.

Var
  NextCode: Array[0..MAX_BITS] Of Word; // next code value for each bit length
  Code: Word;      // running code value
  Bits: Integer;   // bit Index
  N: Integer;      // code Index
  Len: Integer;

Begin
  Code := 0;

  // The distribution counts are first used to generate the code values without bit reversal.
  For Bits := 1 To MAX_BITS Do
  Begin
    Code := (Code + BitLengthCounts[Bits - 1]) Shl 1;
    NextCode[Bits] := Code;
  End;

  // Check that the bit counts in BitLengthCounts are consistent. The last code must be all ones.
  For N := 0 To MaxCode Do
  Begin
    Len := Tree[N].dl.Len;
    If Len = 0 Then Continue;
    Tree[N].fc.Code := BitReverse(NextCode[Len], Len);
    Inc(NextCode[Len]);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure InitializeBlock(Var S: TDeflateState);

Var
  N: Integer;  

Begin
  // initialize the trees 
  For N := 0 To L_CODES - 1 Do S.LiteralTree[N].fc.Frequency := 0;
  For N := 0 To D_CODES - 1 Do S.DistanceTree[N].fc.Frequency := 0;
  For N := 0 To BL_CODES - 1 Do S.BitLengthTree[N].fc.Frequency := 0;

  S.LiteralTree[END_BLOCK].fc.Frequency := 1;
  S.StaticLength := 0;
  S.OptimalLength := 0;
  S.Matches := 0;
  S.LastLiteral := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TreeInit(Var S: TDeflateState);

// initializes the tree data structures for a new zlib stream

Begin
  S.CompressedLength := 0;

  S.LiteralDescriptor.DynamicTree := @S.LiteralTree;
  S.LiteralDescriptor.StaticDescriptor := @StaticLiteralDescriptor;

  S.DistanceDescriptor.DynamicTree := @S.DistanceTree;
  S.DistanceDescriptor.StaticDescriptor := @StaticDistanceDescriptor;

  S.BitLengthDescriptor.DynamicTree := @S.BitLengthTree;
  S.BitLengthDescriptor.StaticDescriptor := @StaticBitLengthDescriptor;

  S.BitsBuffer := 0;
  S.ValidBits := 0;
  S.LastEOBLength := 8; // enough Lookahead for Inflate 
  // initialize the first block of the first file 
  InitializeBlock(S);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure RestoreHeap(Var S: TDeflateState; Const Tree: TTree; K: Integer);

// Restores the heap property by moving down tree starting at node K,
// exchanging a Node with the smallest of its two sons if necessary, stopping
// when the heap property is re-established (each father smaller than its two sons).

Var
  V, J: Integer;

Begin
  V := S.Heap[K];
  J := K Shl 1;  // left son of K
  While J <= S.HeapLength Do
  Begin
    // set J to the smallest of the two sons:
    If (J < S.HeapLength) And
       ((Tree[S.Heap[J + 1]].fc.Frequency < Tree[S.Heap[J]].fc.Frequency) Or
        ((Tree[S.Heap[J + 1]].fc.Frequency = Tree[S.Heap[J]].fc.Frequency) And
         (S.Depth[S.Heap[J + 1]] <= S.Depth[S.Heap[J]]))) Then Inc(J);

    // exit if V is smaller than both sons
    If ((Tree[V].fc.Frequency < Tree[S.Heap[J]].fc.Frequency) Or
       ((Tree[V].fc.Frequency = Tree[S.Heap[J]].fc.Frequency) And
        (S.Depth[V] <= S.Depth[S.Heap[J]]))) Then Break;

    // exchange V with the smallest son
    S.Heap[K] := S.Heap[J];
    K := J;

    // and xontinue down the tree, setting J to the left son of K
    J := J Shl 1;
  End;
  S.Heap[K] := V;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure GenerateBitLengths(Var S: TDeflateState; Var Descriptor: TTreeDescriptor);

// Computes the optimal bit lengths for a tree and update the total bit length for the current block.
// The fields Frequency and dad are set, Heap[HeapMaximum] and above are the tree nodes sorted by increasing frequency.
//
// Result: The field Len is set to the optimal bit length, the array BitLengthCounts contains the frequencies for each
// bit length. The length OptimalLength is updated. StaticLength is also updated if STree is not nil.

Var
  Tree: PTree;
  MaxCode: Integer;
  STree: PTree;
  Extra: PIntegerArray;
  Base: Integer;
  MaxLength: Integer;
  H: Integer;          // heap Index
  N, M: Integer;       // iterate over the tree elements
  Bits: Word;          // bit length
  ExtraBits: Integer;
  F: Word;             // frequency
  Overflow: Integer;   // number of elements with bit length too large 
  
Begin
  Tree := Descriptor.DynamicTree;
  MaxCode := Descriptor.MaxCode;
  STree := Descriptor.StaticDescriptor.StaticTree;
  Extra := Descriptor.StaticDescriptor.ExtraBits;
  Base := Descriptor.StaticDescriptor.ExtraBase;
  MaxLength := Descriptor.StaticDescriptor.MaxLength;
  Overflow := 0;

  FillChar(S.BitLengthCounts, SizeOf(S.BitLengthCounts), 0);

  // in a first pass, compute the optimal bit lengths (which may overflow in the case of the bit length tree) 
  Tree[S.Heap[S.HeapMaximum]].dl.Len := 0; // root of the heap 

  For H := S.HeapMaximum + 1 To HEAP_SIZE - 1 Do
  Begin
    N := S.Heap[H];
    Bits := Tree[Tree[N].dl.Dad].dl.Len + 1;
    If Bits > MaxLength Then
    Begin
      Bits := MaxLength;
      Inc(Overflow);
    End;
    Tree[N].dl.Len := Bits;

    // overwrite Tree[N].dl.Dad which is no longer needed
    If N > MaxCode Then Continue; // not a leaf node 

    Inc(S.BitLengthCounts[Bits]);
    ExtraBits := 0;
    If N >= Base Then ExtraBits := Extra[N - Base];
    F := Tree[N].fc.Frequency;
    Inc(S.OptimalLength, Integer(F) * (Bits + ExtraBits));
    If Assigned(STree) Then Inc(S.StaticLength, Integer(F) * (STree[N].dl.Len + ExtraBits));
  End;
  // This happens for example on obj2 and pic of the Calgary corpus 
  If Overflow = 0 Then Exit;

  // find the first bit length which could increase 
  Repeat
    Bits := MaxLength - 1;
    While (S.BitLengthCounts[Bits] = 0) Do Dec(Bits);
    // move one leaf down the tree
    Dec(S.BitLengthCounts[Bits]);
    // move one overflow item as its brother
    Inc(S.BitLengthCounts[Bits + 1], 2);
    // The brother of the overflow item also moves one step up,
    // but this does not affect BitLengthCounts[MaxLength]
    Dec(S.BitLengthCounts[MaxLength]);
    Dec(Overflow, 2);
  Until (Overflow <= 0);

  // Now recompute all bit lengths, scanning in increasing frequency.
  // H is still equal to HEAP_SIZE. (It is simpler to reconstruct all
  // lengths instead of fixing only the wrong ones. This idea is taken
  // from 'ar' written by Haruhiko Okumura.)
  H := HEAP_SIZE;
  For Bits := MaxLength DownTo 1 Do
  Begin
    N := S.BitLengthCounts[Bits];
    While (N <> 0) Do
    Begin
      Dec(H);
      M := S.Heap[H];
      If M > MaxCode Then Continue;
      If Tree[M].dl.Len <> Bits Then
      Begin
        Inc(S.OptimalLength, (Bits - Tree[M].dl.Len) * Tree[M].fc.Frequency);
        Tree[M].dl.Len := Word(Bits);
      End;
      Dec(N);
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure BuildTree(Var S: TDeflateState; Var Descriptor: TTreeDescriptor);

// Constructs a Huffman tree and assigns the code bit strings and lengths.
// Updates the total bit length for the current block. The field Frequency must be set for all tree elements on entry.
//
// Result: the fields Len and Code are set to the optimal bit length and corresponding Code. The length OptimalLength
// is updated; StaticLength is also updated if STree is not nil. The field MaxCode is set.

Var
  Tree: PTree;
  STree: PTree;
  Elements: Integer;
  N, M: Integer;    // iterate over heap elements
  MaxCode: Integer; // largest code with non zero frequency
  Node: Integer;    // new node being created 

Begin
  Tree := Descriptor.DynamicTree;
  STree := Descriptor.StaticDescriptor.StaticTree;
  Elements := Descriptor.StaticDescriptor.Elements;
  MaxCode := -1;

  // Construct the initial Heap, with least frequent element in Heap[SMALLEST].
  // The sons of Heap[N] are Heap[2 * N] and Heap[2 * N + 1]. Heap[0] is not used. 
  S.HeapLength := 0;
  S.HeapMaximum := HEAP_SIZE;

  For N := 0 To Elements - 1 Do
  Begin
    If Tree[N].fc.Frequency = 0 Then Tree[N].dl.Len := 0
                                Else
    Begin
      MaxCode := N;
      Inc(S.HeapLength);
      S.Heap[S.HeapLength] := N;
      S.Depth[N] := 0;
    End;
  End;

  // The pkzip format requires that at least one distance code exists and that at least one bit
  // should be sent even if there is only one possible code. So to avoid special checks later on we force at least
  // two codes of non zero frequency.
  While S.HeapLength < 2 Do
  Begin
    Inc(S.HeapLength);
    If MaxCode < 2 Then
    Begin
      Inc(MaxCode);
      S.Heap[S.HeapLength] := MaxCode;
      Node := MaxCode;
    End
    Else
    Begin
      S.Heap[S.HeapLength] := 0;
      Node := 0;
    End;
    Tree[Node].fc.Frequency := 1;
    S.Depth[Node] := 0;
    Dec(S.OptimalLength);
    If (STree <> Nil) Then Dec(S.StaticLength, STree[Node].dl.Len);
    // Node is 0 or 1 so it does not have extra bits 
  End;
  Descriptor.MaxCode := MaxCode;

  // The elements Heap[HeapLength / 2 + 1 .. HeapLength] are leaves of the Tree,
  // establish sub-heaps of increasing lengths.
  For N := S.HeapLength Div 2 DownTo 1 Do RestoreHeap(S, Tree^, N);

  // construct the Huffman tree by repeatedly combining the least two frequent nodes
  Node := Elements; // next internal node of the tree
  Repeat
    N := S.Heap[SMALLEST];
    S.Heap[SMALLEST] := S.Heap[S.HeapLength];
    Dec(S.HeapLength);
    RestoreHeap(S, Tree^, SMALLEST);

    // M := node of next least frequency
    M := S.Heap[SMALLEST];
    Dec(S.HeapMaximum);
    // keep the nodes sorted by frequency
    S.Heap[S.HeapMaximum] := N;
    Dec(S.HeapMaximum);
    S.Heap[S.HeapMaximum] := M;

    // create a new node father of N and M
    Tree[Node].fc.Frequency := Tree[N].fc.Frequency + Tree[M].fc.Frequency;
    // maximum
    If (S.Depth[N] >= S.Depth[M]) Then S.Depth[Node] := Byte (S.Depth[N] + 1)
                                  Else S.Depth[Node] := Byte (S.Depth[M] + 1);

    Tree[M].dl.Dad := Word(Node);
    Tree[N].dl.Dad := Word(Node);
    // and insert the new node in the heap
    S.Heap[SMALLEST] := Node;
    Inc(Node);
    RestoreHeap(S, Tree^, SMALLEST);
  Until S.HeapLength < 2;

  Dec(S.HeapMaximum);
  S.Heap[S.HeapMaximum] := S.Heap[SMALLEST];

  // At this point the fields Frequency and dad are set. We can now generate the bit lengths.
  GenerateBitLengths(S, Descriptor);

  // The field Len is now set, we can generate the bit codes 
  GenerateCodes(Tree, MaxCode, S.BitLengthCounts);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure ScanTree(Var S: TDeflateState; Var Tree: Array Of TTreeEntry; MaxCode: Integer);

// Scans a given tree to determine the frequencies of the codes in the bit length tree.
// MaxCode is the tree's largest code of non zero frequency.

Var
  N: Integer;           // iterates over all tree elements
  PreviousLen: Integer; // last emitted length
  CurrentLen: Integer;  // Length of current code
  NextLen: Integer;     // length of next code
  Count: Integer;       // repeat count of the current xode
  MaxCount: Integer;    // max repeat count
  MinCount: Integer;    // min repeat count
   
Begin
  PreviousLen := -1;
  NextLen := Tree[0].dl.Len;
  Count := 0;
  MaxCount := 7;
  MinCount := 4;

  If NextLen = 0 Then
  Begin
    MaxCount := 138;
    MinCount := 3;
  End;
  Tree[MaxCode + 1].dl.Len := Word($FFFF); // guard

  For N := 0 To MaxCode Do
  Begin
    CurrentLen := NextLen;
    NextLen := Tree[N + 1].dl.Len;
    Inc(Count);
    If (Count < MaxCount) And (CurrentLen = NextLen) Then Continue
                                                     Else
      If (Count < MinCount) Then Inc(S.BitLengthTree[CurrentLen].fc.Frequency, Count)
                            Else
        If CurrentLen <> 0 Then
        Begin
          If (CurrentLen <> PreviousLen) Then Inc(S.BitLengthTree[CurrentLen].fc.Frequency);
          Inc(S.BitLengthTree[REP_3_6].fc.Frequency);
        End
        Else
          If (Count <= 10) Then Inc(S.BitLengthTree[REPZ_3_10].fc.Frequency)
                           Else Inc(S.BitLengthTree[REPZ_11_138].fc.Frequency);
    Count := 0;
    PreviousLen := CurrentLen;
    If NextLen = 0 Then
    Begin
      MaxCount := 138;
      MinCount := 3;
    End
    Else
      If CurrentLen = NextLen Then
      Begin
        MaxCount := 6;
        MinCount := 3;
      End
      Else
      Begin
        MaxCount := 7;
        MinCount := 4;
      End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure SendTree(Var S: TDeflateState; Const Tree: Array Of TTreeEntry; MaxCode: Integer);

// Sends the given tree in compressed form using the codes in BitLengthTree. 
// MaxCode is the tree's largest code of non zero frequency.

Var
  N: Integer;           // iterates over all tree elements
  PreviousLen: Integer; // last emitted length
  CurrentLen: Integer;  // length of current code
  NextLen: Integer;     // length of next code
  Count: Integer;       // repeat count of the current code
  MaxCount: Integer;    // max repeat count 
  MinCount: Integer;    // min repeat count

Begin
  PreviousLen := -1;
  NextLen := Tree[0].dl.Len;
  Count := 0;
  MaxCount := 7;
  MinCount := 4;

  // guard is already set 
  If NextLen = 0 Then
  Begin
    MaxCount := 138;
    MinCount := 3;
  End;

  For N := 0 To MaxCode Do
  Begin
    CurrentLen := NextLen;
    NextLen := Tree[N + 1].dl.Len;
    Inc(Count);
    If (Count < MaxCount) And (CurrentLen = NextLen) Then Continue
                                                     Else
      If Count < MinCount Then
      Begin
        Repeat
          SendBits(S, S.BitLengthTree[CurrentLen].fc.Code, S.BitLengthTree[CurrentLen].dl.Len);
          Dec(Count);
        Until (Count = 0);
      End
      Else
        If CurrentLen <> 0 Then
        Begin
          If CurrentLen <> PreviousLen Then
          Begin
            SendBits(S, S.BitLengthTree[CurrentLen].fc.Code, S.BitLengthTree[CurrentLen].dl.Len);
            Dec(Count);
          End;
          SendBits(S, S.BitLengthTree[REP_3_6].fc.Code, S.BitLengthTree[REP_3_6].dl.Len);
          SendBits(S, Count - 3, 2);
        End
        Else
          If Count <= 10 Then
          Begin
            SendBits(S, S.BitLengthTree[REPZ_3_10].fc.Code, S.BitLengthTree[REPZ_3_10].dl.Len);
            SendBits(S, Count - 3, 3);
          End
          Else
          Begin
            SendBits(S, S.BitLengthTree[REPZ_11_138].fc.Code, S.BitLengthTree[REPZ_11_138].dl.Len);
            SendBits(S, Count - 11, 7);
          End;
    Count := 0;
    PreviousLen := CurrentLen;
    If NextLen = 0 Then
    Begin
      MaxCount := 138;
      MinCount := 3;
    End
    Else
      If CurrentLen = NextLen Then
      Begin
        MaxCount := 6;
        MinCount := 3;
      End
      Else
      Begin
        MaxCount := 7;
        MinCount := 4;
      End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function BuildBitLengthTree(Var S: TDeflateState): Integer;

// Constructs the Huffman tree for the bit lengths and returns the Index in BitLengthOrder
// of the last bit length code to send.

Begin
  // determine the bit length frequencies for literal and distance trees
  ScanTree(S, S.LiteralTree, S.LiteralDescriptor.MaxCode);
  ScanTree(S, S.DistanceTree, S.DistanceDescriptor.MaxCode);

  // build the bit length tree
  BuildTree(S, S.BitLengthDescriptor);
  // OptimalLength now includes the length of the tree representations, except
  // the lengths of the bit lengths codes and the 5 + 5 + 4 (= 14) bits for the counts.

  // Determine the number of bit length codes to send. The pkzip format requires that at least 4 bit length codes
  // be sent. (appnote.txt says 3 but the actual value used is 4.)
  For Result := BL_CODES - 1 DownTo 3 Do
    If S.BitLengthTree[BitLengthOrder[Result]].dl.Len <> 0 Then Break;

  // update OptimalLength to include the bit length tree and counts 
  Inc(S.OptimalLength, 3 * (Result + 1) + 14);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure SendAllTrees(Var S: TDeflateState; lcodes, dcodes, blcodes: Integer);

// Sends the header for a block using dynamic Huffman trees: the counts, the
// lengths of the bit length codes, the literal tree and the distance tree.
// lcodes must be >= 257, dcodes >= 1 and blcodes >= 4

Var
  Rank: Integer;          

Begin
  SendBits(S, lcodes - 257, 5); // not +255 as stated in appnote.txt
  SendBits(S, dcodes - 1,   5);
  SendBits(S, blcodes - 4,  4); // not -3 as stated in appnote.txt

  For Rank := 0 To blcodes - 1 Do SendBits(S, S.BitLengthTree[BitLengthOrder[Rank]].dl.Len, 3);
  SendTree(S, S.LiteralTree, lcodes-1);
  SendTree(S, S.DistanceTree, dcodes-1);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure BitsWindup(Var S: TDeflateState);

// flushs the bit buffer and aligns the output on a byte boundary

Begin
  If S.ValidBits > 8 Then
  Begin
    S.PendingBuffer[S.Pending] := Byte(S.BitsBuffer And $FF);
    Inc(S.Pending);
    S.PendingBuffer[S.Pending] := Byte(Word(S.BitsBuffer) Shr 8);;
    Inc(S.Pending);
  End
  Else
    If S.ValidBits > 0 Then
    Begin
      S.PendingBuffer[S.Pending] := Byte(S.BitsBuffer);
      Inc(S.Pending);
    End;
    
  S.BitsBuffer := 0;
  S.ValidBits := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure CopyBlock(Var S: TDeflateState; Buffer: PByte; Len: Cardinal; Header: Boolean);

// copies a stored block, storing first the length and its one's complement if requested
// Buffer contains the input data, Len the buffer length and Header is True if the block Header must be written too.

Begin
  BitsWindup(S);        // align on byte boundary
  S.LastEOBLength := 8; // enough lookahead for Inflate

  If Header Then
  Begin
    S.PendingBuffer[S.Pending] := Byte(Word(Len) And $FF);
    Inc(S.Pending);
    S.PendingBuffer[S.Pending] := Byte(Word(Len) Shr 8);
    Inc(S.Pending);
    S.PendingBuffer[S.Pending] := Byte(Word(Not Len) And $FF);
    Inc(S.Pending);
    S.PendingBuffer[S.Pending] := Byte(Word(Not Len) Shr 8);
    Inc(S.Pending);
  End;

  While Len > 0 Do
  Begin
    Dec(Len);
    S.PendingBuffer[S.Pending] := Buffer^;
    Inc(Buffer);
    Inc(S.Pending);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TreeStroredBlock(Var S: TDeflateState; Buffer: PByte; StoredLength: Integer; EOF: Boolean);

// sends a stored block
// Buffer contains the input data, Len the buffer length and EOF is True if this is the last block for a file.

Begin
  SendBits(S, (STORED_BLOCK Shl 1) + Ord(EOF), 3);  // send block type 
  S.CompressedLength := (S.CompressedLength + 10) And Integer(Not 7);
  Inc(S.CompressedLength, (StoredLength + 4) Shl 3);

  // copy with header
  CopyBlock(S, Buffer, Cardinal(StoredLength), True);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure BitsFlush(Var S: TDeflateState);

// flushs the bit buffer, keeping at most 7 bits in it

Begin
  If S.ValidBits = 16 Then
  Begin
    S.PendingBuffer[S.Pending] := Byte(S.BitsBuffer And $FFf);
    Inc(S.Pending);
    S.PendingBuffer[S.Pending] := Byte(Word(S.BitsBuffer) Shr 8);
    Inc(S.Pending);

    S.BitsBuffer := 0;
    S.ValidBits := 0;
  End
  Else
   If S.ValidBits >= 8 Then
   Begin
     S.PendingBuffer[S.Pending] := Byte(S.BitsBuffer);
     Inc(S.Pending);

     S.BitsBuffer := S.BitsBuffer Shr 8;
     Dec(S.ValidBits, 8);
   End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TreeAlign(Var S: TDeflateState);

// Sends one empty static block to give enough lookahead for Inflate. This takes 10 Bits, of which 7 may remain
// in the bit buffer. The current Inflate code requires 9 Bits of lookahead. if the last two codes for the previous
// block (real code plus EOB) were coded on 5 Bits or less, Inflate may have only 5 + 3 Bits of lookahead to decode the 
// last real code. In this case we send two empty static blocks instead of one. (There are no problems if the previous
// block is stored or fixed.) To simplify the code, we assume the worst case of last real code encoded on one bit only.

Begin
  SendBits(S, STATIC_TREES Shl 1, 3);
  SendBits(S, StaticLiteralTree[END_BLOCK].fc.Code, StaticLiteralTree[END_BLOCK].dl.Len);
  Inc(S.CompressedLength, 10); // 3 for block type, 7 for EOB 
  BitsFlush(S);
  // Of the 10 Bits for the empty block, we have already sent
  // (10 - ValidBits) bits. The lookahead for the last real code (before
  // the EOB of the previous block) was thus at least one plus the length
  // of the EOB plus what we have just sent of the empty static block.
  If (1 + S.LastEOBLength + 10 - S.ValidBits) < 9 Then
  Begin
    SendBits(S, STATIC_TREES Shl 1, 3);
    SendBits(S, StaticLiteralTree[END_BLOCK].fc.Code, StaticLiteralTree[END_BLOCK].dl.Len);
    Inc(S.CompressedLength, 10);
    BitsFlush(S);
  End;
  S.LastEOBLength := 7;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure SetDataType(Var S: TDeflateState);

// Sets the data type to ASCII or BINARY, using a crude approximation. Binary if more than 20% of the bytes are
// <= 6 or >= 128, ASCII otherwise. The fields Frequency of LiteralTree are set and the total of all frequencies does
// not exceed 64K.

Var
  N: Integer;
  ASCIIFrequency: Cardinal;
  BinaryFrequency: Cardinal;

Begin
  N := 0;
  ASCIIFrequency := 0;
  BinaryFrequency := 0;

  While N < 7 Do
  Begin
    Inc(BinaryFrequency, S.LiteralTree[N].fc.Frequency);
    Inc(N);
  End;
  While N < 128 Do
  Begin
    Inc(ASCIIFrequency, S.LiteralTree[N].fc.Frequency);
    Inc(N);
  End;
  While N < LITERALS Do
  Begin
    Inc(BinaryFrequency, S.LiteralTree[N].fc.Frequency);
    Inc(N);
  End;

  If BinaryFrequency > (ASCIIFrequency Shr 2) Then S.DataType := Z_BINARY
                                              Else S.DataType := Z_ASCII;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure CompressBlock(Var S: TDeflateState; Const LiteralTree, DistanceTree: Array Of TTreeEntry);

// sends the block data compressed using the given Huffman trees

Var
  Distance: Cardinal; // distance of matched string
  lc: Integer;        // match length or unmatched char (if Distance = 0)
  I: Cardinal;
  Code: Cardinal;     // the code to send
  Extra: Integer;     // number of extra bits to send 

Begin
  I := 0;
  If S.LastLiteral <> 0 Then
  Repeat
    Distance := S.DistanceBuffer[I];
    lc := S.LiteralBuffer[I];
    Inc(I);
    If Distance = 0 Then
    Begin
      // send a literal byte
      SendBits(S, LiteralTree[lc].fc.Code, LiteralTree[lc].dl.Len);
    End
    Else
    Begin
      // Here, lc is the match length - MIN_MATCH
      Code := LengthCode[lc];
      // send the length code 
      SendBits(S, LiteralTree[Code + LITERALS + 1].fc.Code, LiteralTree[Code + LITERALS + 1].dl.Len);
      Extra := ExtraLengthBits[Code];
      If Extra <> 0 Then
      Begin
        Dec(lc, BaseLength[Code]);
        // send the extra length bits
        SendBits(S, lc, Extra);
      End;
      Dec(Distance); // Distance is now the match distance - 1
      If Distance < 256 Then Code := DistanceCode[Distance]
                        Else Code := DistanceCode[256 + (Distance Shr 7)];

      // send the distance code
      SendBits(S, DistanceTree[Code].fc.Code, DistanceTree[Code].dl.Len);
      Extra := ExtraDistanceBits[Code];
      If Extra <> 0 Then
      Begin
        Dec(Distance, BaseDistance[Code]);
        SendBits(S, Distance, Extra);   // send the extra distance bits
      End;
    End; // literal or match pair? 

    // Check that the overlay between PendingBuffer and DistanceBuffer + LiteralBuffer is ok
  Until I >= S.LastLiteral;

  SendBits(S, LiteralTree[END_BLOCK].fc.Code, LiteralTree[END_BLOCK].dl.Len);
  S.LastEOBLength := LiteralTree[END_BLOCK].dl.Len;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TreeFlushBlock(Var S: TDeflateState; Buffer: PByte; StoredLength: Integer; EOF: Boolean): Integer;

// Determines the best encoding for the current block: dynamic trees, static trees or store, and outputs the encoded
// block. Buffer contains the input block (or nil if too old), StoredLength the length of this block and EOF if this
// is the last block.
// Returns the total compressed length so far.

Var
  OptimalByteLength,
  StaticByteLength: Integer; // OptimalLength and StaticLength in bytes
  MacBLIndex: Integer;  // index of last bit length code of non zero frequency 

Begin
  MacBLIndex := 0;

  // build the Huffman trees unless a stored block is forced
  If S.Level > 0 Then
  Begin
    // check if the file is ASCII or binary
    If S.DataType = Z_UNKNOWN Then SetDataType(S);

    // construct the literal and distance trees
    // After this, OptimalLength and StaticLength are the total bit lengths of
    // the compressed block data, excluding the tree representations.
    BuildTree(S, S.LiteralDescriptor);
    BuildTree(S, S.DistanceDescriptor);

    // Build the bit length tree for the above two trees and get the index
    // in BitLengthOrder of the last bit length code to send.
    MacBLIndex := BuildBitLengthTree(S);

    // determine the best encoding, compute first the block length in bytes
    OptimalByteLength := (S.OptimalLength + 10) Shr 3;
    StaticByteLength := (S.StaticLength + 10) Shr 3;
    If StaticByteLength <= OptimalByteLength Then OptimalByteLength := StaticByteLength;
  End
  Else
  Begin
    StaticByteLength := StoredLength + 5;
    OptimalByteLength := StaticByteLength; // force a stored block 
  End;

  // if Iompression failed and this is the first and last block,
  // and if the .zip file can be seeked (to rewrite the local header),
  // the whole file is transformed into a stored file.  
  // (4 are the two words for the lengths) 
  If (StoredLength + 4 <= OptimalByteLength) And Assigned(Buffer) Then
  Begin
    // The test Buffer <> nil is only necessary if LiteralBufferSize > WSize.
    // Otherwise we can't have processed more than WSize input bytes since
    // the last block dlush, because compression would have been successful.
    // if LiteralBufferSize <= WSize, it is never too late to transform a block into a stored block. 
    TreeStroredBlock(S, Buffer, StoredLength, EOF);
  End
  Else
    If StaticByteLength >= 0 Then
    Begin
      // force static trees 
      SendBits(S, (STATIC_TREES Shl 1) + Ord(EOF), 3);
      CompressBlock(S, StaticLiteralTree, StaticDescriptorTree);
      Inc(S.CompressedLength, 3 + S.StaticLength);
    End
    Else
    Begin
      SendBits(S, (DYN_TREES Shl 1) + Ord(EOF), 3);
      SendAllTrees(S, S.LiteralDescriptor.MaxCode + 1, S.DistanceDescriptor.MaxCode + 1, MacBLIndex + 1);
      CompressBlock(S, S.LiteralTree, S.DistanceTree);
      Inc(S.CompressedLength, 3 + S.OptimalLength);
    End;
  InitializeBlock(S);

  If EOF Then
  Begin
    BitsWindup(S);
    // align on byte boundary
    Inc(S.CompressedLength, 7);
  End;

  Result := S.CompressedLength Shr 3;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TreeTally(Var S: TDeflateState; Distance: Cardinal; lc: Cardinal): Boolean;

// Saves the match info and tallies the frequency counts. Returns True if the current block must be flushed.
// Distance is the distance of the matched string and lc either match length minus MIN_MATCH or the unmatch character
// (if Distance = 0).

Var
  Code: Word;

Begin
  S.DistanceBuffer[S.LastLiteral] := Word(Distance);
  S.LiteralBuffer[S.LastLiteral] := Byte(lc);
  Inc(S.LastLiteral);
  If (Distance = 0) Then
  Begin
    // lc is the unmatched char
    Inc(S.LiteralTree[lc].fc.Frequency);
  End
  Else
  Begin
    Inc(S.Matches);
    // here, lc is the match length - MIN_MATCH
    Dec(Distance);              
    If Distance < 256 Then Code := DistanceCode[Distance]
                      Else Code := DistanceCode[256 + (Distance Shr 7)];
    Inc(S.LiteralTree[LengthCode[lc] + LITERALS + 1].fc.Frequency);
    Inc(S.DistanceTree[Code].fc.Frequency);
  End;

  Result := (S.LastLiteral = S.LiteralBufferSize - 1);
  // We avoid equality with LiteralBufferSize because stored blocks are restricted to 64K - 1 bytes. 
End;

//----------------- deflation support ----------------------------------------------------------------------------------

Type
  TBlockState = (
    bsNeedMore,      // block not completed, need more input or more output
    bsBlockDone,     // block flush performed
    bsFinishStarted, // finish started, need only more output at next Deflate
    bsFinishDone     // finish done, accept no more input or output
  );

Type // compression function, returns the block state after the call
  TCompressFunction = Function(Var S: TDeflateState; Flush: Integer): TBlockState;

Function DeflateStored(Var S: TDeflateState; Flush: Integer): TBlockState; forward;
Function DeflateFast(Var S: TDeflateState; Flush: Integer): TBlockState; forward;
Function DeflateSlow(Var S: TDeflateState; Flush: Integer): TBlockState; forward;

Const
  ZNIL = 0;                     // Tail of hash chains
  TOO_FAR = 4096;               // matches of length 3 are discarded if their distance exceeds TOO_FAR 

Type
  TConfig = Record
   GoodLength: Word;            // reduce lazy search above this match length
   MaxLazy: Word;               // do not perform lazy search above this match length
   NiceLength: Word;            // quit search above this match length
   MaxChain: Word;
   Func: TCompressFunction;
  End;

Const
  // Values for MaxLazyMatch, GoodMatch and MaxChainLength, depending on the desired pack Level (0..9).
  // The values given below have been tuned to exclude worst case performance for pathological files.
  // Better values may be found for specific files.
  ConfigurationTable: Array[0..9] Of TConfig = (
    (GoodLength: 0;  MaxLazy: 0;   NiceLength: 0;   MaxChain: 0;    Func: DeflateStored),  // store only
    (GoodLength: 4;  MaxLazy: 4;   NiceLength: 8;   MaxChain: 4;    Func: DeflateFast), // maximum speed
    (GoodLength: 4;  MaxLazy: 5;   NiceLength: 16;  MaxChain: 8;    Func: DeflateFast),
    (GoodLength: 4;  MaxLazy: 6;   NiceLength: 32;  MaxChain: 32;   Func: DeflateFast),
    (GoodLength: 4;  MaxLazy: 4;   NiceLength: 16;  MaxChain: 16;   Func: DeflateSlow),
    (GoodLength: 8;  MaxLazy: 16;  NiceLength: 32;  MaxChain: 32;   Func: DeflateSlow),
    (GoodLength: 8;  MaxLazy: 16;  NiceLength: 128; MaxChain: 128;  Func: DeflateSlow),
    (GoodLength: 8;  MaxLazy: 32;  NiceLength: 128; MaxChain: 256;  Func: DeflateSlow),
    (GoodLength: 32; MaxLazy: 128; NiceLength: 258; MaxChain: 1024; Func: DeflateSlow),
    (GoodLength: 32; MaxLazy: 258; NiceLength: 258; MaxChain: 4096; Func: DeflateSlow)  // maximum compression
  );

// Note: The deflate code requires MaxLazy >= MIN_MATCH and MaxChain >= 4.
//       For DeflateFast (levels <= 3) good is ignored and lazy has a different meaning.

//----------------------------------------------------------------------------------------------------------------------

Procedure InsertString(Var S: TDeflateState; Str: Cardinal; Var MatchHead: Cardinal);

// Inserts Str into the dictionary and sets MatchHead to the previous head of the hash chain (the most recent string
// with same hash key). All calls to to InsertString are made with consecutive input characters and the first MIN_MATCH
// bytes of Str are valid (except for the last MIN_MATCH - 1 bytes of the input file).
// Returns the previous length of the hash chain.

Begin
  S.InsertHash := ((S.InsertHash Shl S.HashShift) Xor (S.Window[(Str) + (MIN_MATCH - 1)])) And S.HashMask;

  MatchHead := S.Head[S.InsertHash];
  S.Previous[(Str) And S.WindowMask] := MatchHead;
  S.Head[S.InsertHash] := Word(Str);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure LongestMatchInit(Var S: TDeflateState);

// initializes the "longest match" routines for a new zlib stream 

Begin
  S.CurrentWindowSize := 2 * S.WindowSize;

  S.Head[S.HashSize - 1] := ZNIL;
  FillChar(S.Head^, (S.HashSize - 1) * SizeOf(S.Head[0]), 0);

  // set the default configuration parameters 
  S.MaxLazyMatch := ConfigurationTable[S.Level].MaxLazy;
  S.GoodMatch := ConfigurationTable[S.Level].GoodLength;
  S.NiceMatch := ConfigurationTable[S.Level].NiceLength;
  S.MaxChainLength := ConfigurationTable[S.Level].MaxChain;

  S.StringStart := 0;
  S.BlockStart := 0;
  S.Lookahead := 0;
  S.PreviousLength := MIN_MATCH - 1;
  S.MatchLength := MIN_MATCH - 1;
  S.MatchAvailable := False;
  S.InsertHash := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateInit2_(Var ZState: TZState; Level: Integer; imMethod: Byte; AWindowBits: Integer; MemLevel:
  Integer; Strategy: Integer; Const Version: AnsiString; StreamSize: Integer): Integer;

// initializes the hash table (Previous[] will be initialized on the fly)

Var
  S: PDeflateState;
  NoHeader: Integer;
  Overlay: PWordArray;
  // We overlay PendingBuffer and DistanceBuffer + LiteralBuffer. This works since the average
  // output size for (length, distance) codes is <= 24 Bits.

Begin
  NoHeader := 0;
  If (Version  =  '') Or (Version[1] <> ZLIB_VERSION[1]) Or (StreamSize <> SizeOf(TZState)) Then
  Begin
    Result := Z_VERSION_ERROR;
    Exit;
  End;

  ZState.Msg := '';
  If Level  =  Z_DEFAULT_COMPRESSION Then Level := 6;

  If AWindowBits < 0 Then
  Begin
    // undocumented feature: suppress zlib header
    NoHeader := 1;
    AWindowBits := -AWindowBits;
  End;
  
  If (MemLevel < 1) Or
     (MemLevel > MAX_MEM_LEVEL) Or
     (imMethod <> Z_DEFLATED) Or
     (AWindowBits < 8) Or
     (AWindowBits > 15) Or
     (Level < 0) Or
     (Level > 9) Or
     (Strategy < 0) Or
     (Strategy > Z_HUFFMAN_ONLY) Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  Try
    S := AllocMem(SizeOf(TDeflateState));
    ZState.State := PInternalState(S);
    S.ZState := @ZState;

    S.NoHeader := NoHeader;
    S.WindowBits := AWindowBits;
    S.WindowSize := 1 Shl S.WindowBits;
    S.WindowMask := S.WindowSize - 1;

    S.HashBits := MemLevel + 7;
    S.HashSize := 1 Shl S.HashBits;
    S.HashMask := S.HashSize - 1;
    S.HashShift := (S.HashBits + MIN_MATCH - 1) Div MIN_MATCH;

    S.Window := AllocMem(S.WindowSize * 2 * SizeOf(Byte));
    S.Previous := AllocMem(S.WindowSize * SizeOf(Word));
    S.Head := AllocMem(S.HashSize * SizeOf(Word));

    S.LiteralBufferSize := 1 Shl (MemLevel + 6); // 16K elements by default 

    Overlay := AllocMem(S.LiteralBufferSize * SizeOf(Word) + 2);
    S.PendingBuffer := PByteArray(Overlay);
    S.PendingBufferSize := S.LiteralBufferSize * (SizeOf(Word) + 2);

    S.DistanceBuffer := @Overlay[S.LiteralBufferSize Div SizeOf(Word)];
    S.LiteralBuffer := @S.PendingBuffer[(1 + SizeOf(Word)) * S.LiteralBufferSize];

    S.Level := Level;
    S.Strategy := Strategy;
    S.imMethod := imMethod;

    Result := DeflateReset(ZState);
  Except
    ZState.Msg := ErrorMessages[ERROR_BASE - Z_MEM_ERROR];
    // free already allocated data on error
    DeflateEnd(ZState);
    Raise;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateInit2(Var ZState: TZState; Level: Integer; Method: Byte; AWindowBits: Integer; MemLevel: Integer;
  Strategy: Integer): Integer;

// This is another Version of DeflateInit with more compression options. The field
//  NextInput must be initialized before by the caller.
//
// The Method parameter is the compression method. It must be Z_DEFLATED in
// this Version of the library. (Method 9 will allow a 64K history buffer and
// partial block flushes.)
//
// The AWindowBits parameter is the base two logarithm of the window size
// (the size of the history buffer). It should be in the range 8..15 for this
// version of the library (the value 16 will be allowed for method 9). Larger
// values of this parameter result in better compression at the expense of
// memory usage. The default value is 15 if DeflateInit is used instead.
//
// The MemLevel parameter specifies how much memory should be allocated
// for the internal compression State. MemLevel = 1 uses minimum memory but
// is slow and reduces compression ratio; MemLevel = 9 uses maximum memory
// for optimal speed. The default value is 8. 
//
// The strategy parameter is used to tune the compression algorithm. Use the
// Value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for data produced by a
// filter (or predictor), or Z_HUFFMAN_ONLY to force Huffman encoding only (no
// string match). Filtered data consists mostly of small values with a
// somewhat random distribution. In this case, the compression algorithm is
// tuned to compress them better. The effect of Z_FILTERED is to force more
// Huffman coding and less string matching; it is somewhat intermediate
// between Z_DEFAULT and Z_HUFFMAN_ONLY. The strategy parameter only affects
// the compression ratio but not the correctness of the compressed output even
// if it is not set appropriately.
//
// if NextInput is not nil the library will use this buffer to hold also
// some history information; the buffer must either hold the entire input
// data or have at least 1 shl (WindowBits + 1) bytes and be writable. If NextInput
// is nil the library will allocate its own history buffer (and leave NextInput
// nil). NextOutput need not be provided here but must be provided by the
// application for the next call of Deflate.
//
// if the history buffer is provided by the application, NextInput must
// must never be changed by the application since the compressor maintains
// information inside this buffer from call to call; the application
// must provide more input only by increasing AvailableInput. NextInput is always
// reset by the library in this case.
//
// DeflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was
// not enough memory, Z_STREAM_ERROR if a parameter is invalid (such as
// an invalid method). Msg is set to '' if there is no error message.
// DeflateInit2 does not perform any compression: this will be done by
// Deflate. 

Begin
  Result := DeflateInit2_(ZState, Level, Method, AWindowBits, MemLevel, Strategy, ZLIB_VERSION, SizeOf(TZState));
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateInit_(ZState: PZState; Level: Integer; Const Version: String; StreamSize: Integer): Integer;

// Initializes the internal stream state for compression. 
//
// The compression level must be Z_DEFAULT_COMPRESSION or between 0 and 9:
// 1 gives best speed, 9 gives best compression, 0 gives no compression at
// all (the input data is simply copied a block at a time).
// Z_DEFAULT_COMPRESSION requests a default compromise between speed and
// compression (currently equivalent to Level 6).
//
// DeflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
// enough memory, Z_STREAM_ERROR if Level is not a valid compression level,
// Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible
// with the version assumed by the caller (ZLIB_VERSION).
// Msg is set to '' if there is no error message.  DeflateInit does not
// perform any compression, this will be done by Deflate. 

Begin
  If ZState = Nil Then DeflateInit_ := Z_STREAM_ERROR
                  Else DeflateInit_ := DeflateInit2_(ZState^, Level, Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL,
                                         Z_DEFAULT_STRATEGY, Version, StreamSize);
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateInit(Var ZState: TZState; Level: Integer): Integer;

Begin
  DeflateInit := DeflateInit2_(ZState, Level, Z_DEFLATED, MAX_WBITS,
         DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, SizeOf(TZState));
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateSetDictionary(Var ZState: TZState; Dictionary: PByte; DictLength: Cardinal): Integer;

// Initializes the compression dictionary (history buffer) from the given
// byte sequence without producing any compressed output. This function must
// be called immediately after DeflateInit or DeflateInit2, before any call
// of Deflate. The compressor and decompressor must use exactly the same
// dictionary (see InflateSetDictionary).
//
// The dictionary should consist of strings (byte sequences) that are likely
// to be encountered later in the data to be compressed, with the most commonly
// used strings preferably put towards the end of the dictionary. Using a
// dictionary is most useful when the data to be compressed is short and
// can be predicted with good accuracy; the data can then be compressed better
// than with the default empty dictionary. In this version of the library,
// only the last 32K bytes of the dictionary are used.
//
// Upon return of this function ZState.Adler is set to the Adler32 value
// of the dictionary. The decompressor may later use this value to determine
// which dictionary has been used by the compressor. (The Adler32 value
// applies to the whole dictionary even if only a subset of the dictionary is
// actually used by the compressor.)
//
// DeflateSetDictionary returns Z_OK if success or Z_STREAM_ERROR if a
// parameter is invalid (such as nil dictionary) or the stream state
// is inconsistent (for example if Deflate has already been called for this
// stream). DeflateSetDictionary does not perform any compression, this will
// be done by Deflate.

Var
  S: PDeflateState;
  Length: Cardinal;
  N: Cardinal;
  HashHead: Cardinal;
  MaxDistance: Cardinal;

Begin
  Length := DictLength;
  HashHead := 0;

  If (ZState.State  =  Nil) Or
     (Dictionary  =  Nil) Or
     (PDeflateState(ZState.State).Status <> INIT_STATE) Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  S := PDeflateState(ZState.State);
  ZState.Adler := Adler32(ZState.Adler, Dictionary, DictLength);

  If Length < MIN_MATCH Then
  Begin
    Result := Z_OK;
    Exit;
  End;

  MaxDistance := S.WindowSize - MIN_LOOKAHEAD;
  If Length > MaxDistance Then
  Begin
    Length := MaxDistance;
    // use the tail of the dictionary
    Inc(Dictionary, DictLength - Length);
  End;

  Move( Dictionary^ , S.Window^, Length);
  S.StringStart := Length;
  S.BlockStart := Integer(Length);

  // Insert all strings in the hash table (except for the last two bytes).
  // S.Lookahead stays nil, so S.InsertHash will be recomputed at the next call of FillWindow.
  S.InsertHash := S.Window[0];
  S.InsertHash := ((S.InsertHash Shl S.HashShift) Xor (S.Window[1])) And S.HashMask;

  For N := 0 To Length - MIN_MATCH Do InsertString(S^, N, HashHead);

  Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateReset(Var ZState: TZState): Integer;

// This function is equivalent to DeflateEnd followed by DeflateInit,
// but does not free and reallocate all the internal compression state.
// The stream will keep the same compression level and any other attributes
// that may have been set by DeflateInit2.
//
// DeflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
// stream state was inconsistent (such as state being nil).

Var
  S: PDeflateState;

Begin
  If ZState.State = Nil Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  ZState.TotalOutput := 0;
  ZState.TotalInput := 0;
  ZState.Msg := '';       
  ZState.DataType := Z_UNKNOWN;

  S := PDeflateState(ZState.State);
  S.Pending := 0;
  S.PendingOutput := PByte(S.PendingBuffer);

  If S.NoHeader < 0 Then
  Begin
    // was set to -1 by Deflate(..., Z_FINISH);
    S.NoHeader := 0;
  End;
  
  If S.NoHeader <> 0 Then S.Status := BUSY_STATE
                     Else S.Status := INIT_STATE;
  ZState.Adler := 1;
  S.LastFlush := Z_NO_FLUSH;

  TreeInit(S^);
  LongestMatchInit(S^);

  Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateParams(Var ZState: TZState; Level: Integer; Strategy: Integer): Integer;

// Dynamically update the compression level and compression strategy.
// This can be used to switch between compression and straight copy of
// the input data or to switch to a different kind of input data requiring
// a different strategy. If the compression level is changed the input
// available so far is compressed with the old Level (and may be flushed).
// The new level will take effect only at the next call of Deflate.
//
// Before the call of DeflateParams the stream state must be set as for
// a call of Deflate, since the currently available input may have to
// be compressed and flushed. In particular, ZState.AvailableOutput must be non-zero.
//
// DeflateParams returns Z_OK if successuful, Z_STREAM_ERROR if the source
// stream state was inconsistent or if a parameter was invalid, Z_BUF_ERROR
// if ZState.AvailableOutput was zero.

Var
  S: PDeflateState;
  Func: TCompressFunction;
  Error: Integer;

Begin
  Error := Z_OK;
  If ZState.State  = Nil Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  S := PDeflateState(ZState.State);

  If Level = Z_DEFAULT_COMPRESSION Then Level := 6;
  
  If (Level < 0) Or
     (Level > 9) Or
     (Strategy < 0) Or
     (Strategy > Z_HUFFMAN_ONLY) Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  Func := ConfigurationTable[S.Level].Func;

  If (@Func <> @ConfigurationTable[Level].Func) And (ZState.TotalInput <> 0) Then
  Begin
    // flush the last buffer
    Error := Deflate(ZState, Z_PARTIAL_FLUSH);
  End;

  If S.Level <> Level Then
  Begin
    S.Level := Level;
    S.MaxLazyMatch := ConfigurationTable[Level].MaxLazy;
    S.GoodMatch := ConfigurationTable[Level].GoodLength;
    S.NiceMatch := ConfigurationTable[Level].NiceLength;
    S.MaxChainLength := ConfigurationTable[Level].MaxChain;
  End;
  S.Strategy := Strategy;
  Result := Error;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure PutShortMSB(Var S: TDeflateState; B: Cardinal);

// Puts a word in the pending buffer. The 16-bit value is put in MSB order.
// The stream state must be correct and there must be enough room in PendingBuffer.

Begin
  S.PendingBuffer[S.Pending] := B Shr 8;
  Inc(S.Pending);
  S.PendingBuffer[S.Pending] := B And $FF;
  Inc(S.Pending);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure FlushPending(Var ZState: TZState);

// Flushs as much pending output as possible. All Deflate output goes through this function so some applications may
// wish to modify it to avoid allocating a large ZState.NextOutput buffer and copying into it
// (see also ReadBuffer). 

Var
  Len: Cardinal;
  S: PDeflateState;

Begin
  S := PDeflateState(ZState.State);
  Len := S.Pending;

  If Len > ZState.AvailableOutput Then Len := ZState.AvailableOutput;
  If Len > 0 Then
  Begin
    Move(S.PendingOutput^, ZState.NextOutput^, Len);
    Inc(ZState.NextOutput, Len);
    Inc(S.PendingOutput, Len);
    Inc(ZState.TotalOutput, Len);
    Dec(ZState.AvailableOutput, Len);
    Dec(S.Pending, Len);
    If S.Pending = 0 Then S.PendingOutput := PByte(S.PendingBuffer);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function Deflate(Var ZState: TZState; Flush: Integer): Integer;

// Performs one or both of the following actions:
//
// - Compress more input starting at NextInput and update NextInput and AvailableInput
//  accordingly. If not all input can be processed (because there is not enough room in the output buffer), NextInput
//  and AvailableInput are updated and processing will resume at this point for the next call of Deflate.
//
// - Provide more output starting at NextOutput and update NextOutput and AvailableOutput accordingly. This action is
//   forced if the parameter Flush is non zero. Forcing Flush frequently degrades the compression ratio, so this
//   parameter should be set only when necessary (in interactive applications).
//   Some output may be provided even if Flush is not set.
//
// Before the call of Deflate, the application should ensure that at least one of the actions is possible, by providing
// more input and/or consuming more output, and updating AvailableInput or AvailableOutput accordingly. AvailableOutput
// should never be zero before the call. The application can consume the compressed output when it wants, for example
// when the output buffer is full (AvailableOutput = 0), or after each call of Deflate. if Deflate returns Z_OK and with
// zero AvailableOutput, it must be called again after making room in the output buffer because there might be more output pending.
//
// If the parameter Flush is set to Z_PARTIAL_FLUSH, the current compression block is terminated and flushed to the
// output buffer so that the decompressor can get all input data available so far. For method 9 a future variant on
// method 8, the current block will be flushed but not terminated. Z_SYNC_FLUSH has the same effect as partial flush
// except that the compressed output is byte aligned (the compressor can clear its internal bit buffer) and the current
// block is always terminated. This can be useful if the compressor has to be restarted from scratch after an
// interruption (in which case the internal state of the compressor may be lost). If Flush is set to Z_FULL_FLUSH, the
// compression block is terminated, a special marker is output and the compression dictionary is discarded. This
// is useful to allow the decompressor to synchronize if one compressed block has been damaged (see InflateSync below).
// Flushing degrades compression and so should be used only when necessary.  Using Z_FULL_FLUSH too often can seriously
// degrade the compression. if Deflate returns with AvailableOutput = 0, this function must be called again with the
// same Value of the Flush parameter and more output space (updated AvailableOutput), until the Flush is complete
// (Deflate returns with non-zero AvailableOutput).
//
// If the parameter Flush is set to Z_FINISH, all Pending input is processed, all pending output is flushed and Deflate
// returns with Z_STREAM_END if there was enough output space. If Deflate returns with Z_OK, this function must be
// called again with Z_FINISH and more output space (updated AvailableOutput) but no more input data, until it returns
// with Z_STREAM_END or an error. After Deflate has returned Z_STREAM_END, the only possible operations on the
// stream are DeflateReset or DeflateEnd.
//
// Z_FINISH can be used immediately after DeflateInit if all the compression is to be done in a single step. In this
// case, AvailableOutput must be at least 0.1% larger than AvailableInput plus 12 bytes. If Deflate does not return
// Z_STREAM_END then it must be called again as described above.
//
// Deflate may update DataType if it can make a good guess about the input data type (Z_ASCII or Z_BINARY). In doubt,
// the data is considered binary. This field is only for information purposes and does not affect the compression
// algorithm in any manner.
//
// Deflate returns Z_OK if some progress has been made (mnore input processed or more output produced), Z_STREAM_END if
// all input has been consumed and all output has been produced (only when Flush is set to Z_FINISH), Z_STREAM_ERROR if
// the stream State was inconsistent (for example if NextInput or NextOutput was nil), Z_BUF_ERROR if no progress is possible. 

Var
  OldFlush: Integer; // value of Flush param for previous Deflate call
  S: PDeflateState;
  Header: Cardinal;
  LevelFlags: Cardinal;
  BlockState: TBlockState;

Begin
  If (ZState.State = Nil) Or (Flush > Z_FINISH) Or (Flush < 0) Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;
  S := PDeflateState(ZState.State);

  If (ZState.NextOutput = Nil) Or
     ((ZState.NextInput = Nil) And (ZState.AvailableInput <> 0)) Or
     ((S.Status = FINISH_STATE) And (Flush <> Z_FINISH)) Then
  Begin
    ZState.Msg := ErrorMessages[ERROR_BASE - Z_STREAM_ERROR];
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  If ZState.AvailableOutput = 0 Then
  Begin
    ZState.Msg := ErrorMessages[ERROR_BASE - Z_BUF_ERROR];
    Result := Z_BUF_ERROR;
    Exit;
  End;

  // just in case
  S.ZState := @ZState;
  OldFlush := S.LastFlush;
  S.LastFlush := Flush;

  // write the zlib header 
  If S.Status = INIT_STATE Then
  Begin
    Header := (Z_DEFLATED + ((S.WindowBits - 8) Shl 4)) Shl 8;
    LevelFlags := (S.Level - 1) Shr 1;

    If LevelFlags > 3 Then LevelFlags := 3;
    Header := Header Or (LevelFlags Shl 6);
    If (S.StringStart <> 0) Then Header := Header Or PRESET_DICT;
    Inc(Header, 31 - (Header Mod 31));

    S.Status := BUSY_STATE;
    PutShortMSB(S^, Header);

    // save the Adler32 of the preset dictionary 
    If S.StringStart <> 0 Then
    Begin
      PutShortMSB(S^, Cardinal(ZState.Adler Shr 16));
      PutShortMSB(S^, Cardinal(ZState.Adler And $FFFF));
    End;
    ZState.Adler := 1;
  End;

  // flush as much pending output as possible 
  If S.Pending <> 0 Then
  Begin
    FlushPending(ZState);
    If ZState.AvailableOutput = 0 Then
    Begin
      // Since AvailableOutput is 0, Deflate will be called again with
      // more output space, but possibly with both Pending and
       // AvailableInput equal to zero. There won't be anything to do,
      // but this is not an error situation so make sure we
      // return OK instead of BUF_ERROR at next call of Deflate.
      S.LastFlush := -1;
      Result := Z_OK;
      Exit;
    End;

    // Make sure there is something to do and avoid duplicate consecutive
    // flushes. For repeated and useless calls with Z_FINISH, we keep
    // returning Z_STREAM_END instead of Z_BUFF_ERROR.
  End
  Else
    If (ZState.AvailableInput = 0) And
       (Flush <= OldFlush) And
       (Flush <> Z_FINISH) Then
    Begin
      ZState.Msg := ErrorMessages[ERROR_BASE - Z_BUF_ERROR];
      Result := Z_BUF_ERROR;
      Exit;
    End;

  // user must not provide more input after the first FINISH
  If (S.Status = FINISH_STATE) And (ZState.AvailableInput <> 0) Then
  Begin
    ZState.Msg := ErrorMessages[ERROR_BASE - Z_BUF_ERROR];
    Result := Z_BUF_ERROR;
    Exit;
  End;

  // start a new block or continue the current one
  If (ZState.AvailableInput <> 0) Or
     (S.Lookahead <> 0) Or
     ((Flush <> Z_NO_FLUSH) And (S.Status <> FINISH_STATE)) Then
  Begin
    BlockState := ConfigurationTable[S.Level].Func(S^, Flush);
    If (BlockState = bsFinishStarted) Or (BlockState = bsFinishDone) Then S.Status := FINISH_STATE;
    If (BlockState = bsNeedMore) Or (BlockState = bsFinishStarted) Then
    Begin
      // avoid BUF_ERROR next call, see above
      If (ZState.AvailableOutput = 0) Then S.LastFlush := -1;
      Result := Z_OK;
      Exit;
      
      // If Flush <> Z_NO_FLUSH and AvailableOutput = 0, the next call
      // of Deflate should use the same Flush parameter to make sure
      // that the Flush is complete. So we don't have to output an
      // empty block here, this will be done at next call. This also
      // ensures that for a very small output buffer we emit at most
      // one empty block. 
    End;
    If BlockState = bsBlockDone Then
    Begin
      If Flush = Z_PARTIAL_FLUSH Then TreeAlign(S^)
                                 Else
      Begin
        // FULL_FLUSH or SYNC_FLUSH 
        TreeStroredBlock(S^, Nil, 0, False);

        // for a full Flush, this empty block will be recognized as a special marker
        If Flush = Z_FULL_FLUSH Then
        Begin
          // forget history
          S.Head[S.HashSize - 1] := ZNIL;
          FillChar(S.Head^, (S.HashSize - 1) * SizeOf(S.Head[0]), 0);
        End;
      End;

      FlushPending(ZState);
      If ZState.AvailableOutput = 0 Then
      Begin
        // avoid BUF_ERROR at next call, see above
        S.LastFlush := -1;
        Result := Z_OK;
        Exit;
      End;
    End;
  End;

  If Flush <> Z_FINISH Then
  Begin
    Result := Z_OK;
    Exit;
  End;

  If S.NoHeader <> 0 Then
  Begin
    Result := Z_STREAM_END;
    Exit;
  End;

  // write the zlib trailer (Adler32)
  PutShortMSB(S^, Cardinal(ZState.Adler Shr 16));
  PutShortMSB(S^, Cardinal(ZState.Adler And $FFFF));
  FlushPending(ZState);

  // If AvailableOutput is zero the application will call Deflate again to Flush the rest
  // write the trailer only once!
  S.NoHeader := -1;
  If S.Pending <> 0 Then Result := Z_OK
                    Else Result := Z_STREAM_END;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateEnd(Var ZState: TZState): Integer;

// All dynamically allocated data structures for this stream are freed.
// This function discards any unprocessed input and does not Flush any
// pending output.
//
// DeflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
// stream State was inconsistent, Z_DATA_ERROR if the stream was freed
// prematurely (some input or output was discarded). 

Var
  Status: Integer;
  S: PDeflateState;

Begin
  If ZState.State = Nil Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  S := PDeflateState(ZState.State);
  Status := S.Status;
  If (Status <> INIT_STATE) And
     (Status <> BUSY_STATE) And
     (Status <> FINISH_STATE) Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  FreeMem(S.PendingBuffer);
  FreeMem(S.Head);
  FreeMem(S.Previous);
  FreeMem(S.Window);
  FreeMem(S);
  ZState.State := Nil;

  If Status = BUSY_STATE Then Result := Z_DATA_ERROR
                         Else Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateCopy(Dest, Source: PZState): Integer;

// Copies the source state to the destination state.
//
// Sets the destination stream as a complete copy of the source stream. If the source stream is using an application-
// supplied history buffer, a new buffer is allocated for the destination stream.  The compressed output buffer is always
// application-supplied. It's the responsibility of the application to provide the correct values of NextOutput and
// AvailableOutput for the next call of Deflate.
//
// This function can be useful when several compression strategies will be tried, for example when there are several
// ways of pre-processing the input data with a filter. The streams that will be discarded should then be freed by
// calling DeflateEnd. Note that DeflateCopy duplicates the internal compression state which can be quite large, so this
// strategy is slow and can consume lots of memory.
//
// DeflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_STREAM_ERROR if the source stream
// state was inconsistent (such as zalloc being nil). Msg is left unchanged in both source and destination.

Var
  DestState: PDeflateState;
  SourceState: PDeflateState;
  Overlay: PWordArray;

Begin
  If (Source = Nil) Or (Dest = Nil) Or (Source.State = Nil) Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  SourceState := PDeflateState(Source.State);
  Dest^ := Source^;

  Try
    DestState := AllocMem(SizeOf(TDeflateState));

    Dest.State := PInternalState(DestState);
    DestState^ := SourceState^;
    DestState.ZState := Dest;

    DestState.Window := AllocMem(2 * DestState.WindowSize);
    DestState.Previous := AllocMem(DestState.WindowSize * SizeOf(Word));
    DestState.Head := AllocMem(DestState.HashSize * SizeOf(Word));
    Overlay := AllocMem(DestState.LiteralBufferSize * SizeOf(Word) + 2);
    DestState.PendingBuffer := PByteArray (Overlay);

    Move(SourceState.Window^, DestState.Window^, 2 * DestState.WindowSize);
    Move(SourceState.Previous^, DestState.Previous^, DestState.WindowSize * SizeOf(Word));
    Move(SourceState.Head^, DestState.Head^, DestState.HashSize * SizeOf(Word));
    Move(SourceState.PendingBuffer^, DestState.PendingBuffer^, DestState.PendingBufferSize);

    DestState.PendingOutput := @DestState.PendingBuffer[Cardinal(SourceState.PendingOutput) - Cardinal(SourceState.PendingBuffer)];
    DestState.DistanceBuffer := @Overlay[DestState.LiteralBufferSize Div SizeOf(Word)];
    DestState.LiteralBuffer := @DestState.PendingBuffer[(1 + SizeOf(Word)) * DestState.LiteralBufferSize];

    DestState.LiteralDescriptor.DynamicTree := @DestState.LiteralTree;
    DestState.DistanceDescriptor.DynamicTree := @DestState.DistanceTree;
    DestState.BitLengthDescriptor.DynamicTree := @DestState.BitLengthTree;

    Result := Z_OK;
  Except
    DeflateEnd(Dest^);
    Raise;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function ReadBuffer(ZState: PZState; Buffer: PByte; Size: Cardinal): Integer;

// Reads a new buffer from the current input stream, updates the Adler32 and total number of bytes read.  All Deflate
// input goes through this function so some applications may wish to modify it to avoid allocating a large
// ZState.NextInput buffer and copying from it (see also FlushPending).

Var
  Len: Cardinal;

Begin
  Len := ZState.AvailableInput;

  If Len > Size Then Len := Size;
  If Len = 0 Then
  Begin
    Result := 0;
    Exit;
  End;

  Dec(ZState.AvailableInput, Len);

  If PDeflateState(ZState.State).NoHeader = 0 Then ZState.Adler := Adler32(ZState.Adler, ZState.NextInput, Len);
  Move(ZState.NextInput^, Buffer^, Len);
  Inc(ZState.NextInput, Len);
  Inc(ZState.TotalInput, Len);
  Result := Len;
End;

//----------------------------------------------------------------------------------------------------------------------

Function LongestMatch(Var S: TDeflateState; CurrentMatch: Cardinal): Cardinal;

// Sets MatchStart to the longest match starting at the given string and returns its length. Matches shorter or equal to
// PreviousLength are discarded, in which case the result is equal to PreviousLength and MatchStart is garbage.
// CurrentMatch is the head of the hash chain for the current string (StringStart) and its distance is <= MaxDistance,
// and PreviousLength >= 1.
// The match length will not be greater than S.Lookahead. 

Var
  ChainLength: Cardinal; // max hash chain length
  Scan: PByte;           // current string
  Match: PByte;          // matched string
  Len: Cardinal;         // length of current match
  BestLen: Cardinal;     // best match length so far
  NiceMatch: Cardinal;    
  Limit: Cardinal;

  Previous: PWordArray;
  WMask: Cardinal;
  StrEnd: PByte;
  ScanEnd1: Byte;
  ScanEnd: Byte;
  MaxDistance: Cardinal;

Begin
  ChainLength := S.MaxChainLength;
  Scan := @S.Window[S.StringStart];
  BestLen := S.PreviousLength;
  NiceMatch := S.NiceMatch;
  MaxDistance := S.WindowSize - MIN_LOOKAHEAD;

  // In order to simplify the code, match distances are limited to MaxDistance instead of WSize.
  If S.StringStart > MaxDistance Then Limit := S.StringStart - MaxDistance
                                 Else Limit := ZNIL;

  // Stop when CurrentMatch becomes <= Limit. To simplify the Code we prevent matches with the string of window index 0.
  Previous := S.Previous;
  WMask := S.WindowMask;

  StrEnd := @S.Window[S.StringStart + MAX_MATCH];
  {$IFOPT R+} {$R-} {$DEFINE RangeCheck} {$ENDIF}
  ScanEnd1 := PByteArray(Scan)[BestLen - 1];
  ScanEnd := PByteArray(Scan)[BestLen];
  {$IFDEF RangeCheck} {$R+} {$UNDEF RangeCheck} {$ENDIF}

  // The code is optimized for HashBits >= 8 and MAX_MATCH - 2 multiple of 16.
  // It is easy to get rid of this optimization if necessary.
  // Do not waste too much time if we already have a good Match.
  If S.PreviousLength >= S.GoodMatch Then ChainLength := ChainLength Shr 2;

  // Do not look for matches beyond the end of the input. This is necessary to make Deflate deterministic.
  If NiceMatch > S.Lookahead Then NiceMatch := S.Lookahead;

  Repeat
    Match := @S.Window[CurrentMatch];

    // Skip to next match if the match length cannot increase or if the match length is less than 2.
    {$IFOPT R+} {$R-} {$DEFINE RangeCheck} {$ENDIF}
    If (PByteArray(Match)[BestLen] = ScanEnd) And
       (PByteArray(Match)[BestLen - 1] = ScanEnd1) And
       (Match^ = Scan^) Then
    {$IFDEF RangeCheck} {$R+} {$UNDEF RangeCheck} {$ENDIF}
    Begin
      Inc(Match);
      If Match^ <> PByteArray(Scan)[1] Then
      Begin
        // The Check at BestLen - 1 can be removed because it will be made again later (this heuristic is not always a win).
        // It is not necessary to compare Scan[2] and Match[2] since they are always equal when the other bytes match,
        // given that the hash keys are equal and that HashBits >= 8.
        Inc(Scan, 2);
        Inc(Match);

        // We check for insufficient lookahead only every 8th comparison, the 256th check will be made at StringStart + 258.
        Repeat
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
          Inc(Scan); Inc(Match); If (Scan^ <> Match^) Then Break;
        Until (Cardinal(Scan) >= Cardinal(StrEnd));

        Len := MAX_MATCH - Integer(Cardinal(StrEnd) - Cardinal(Scan));
        Scan := StrEnd;
        Dec(Scan, MAX_MATCH);

        If Len > BestLen Then
        Begin
          S.MatchStart := CurrentMatch;
          BestLen := Len;
          If Len >= NiceMatch Then Break;
          {$IFOPT R+} {$R-} {$DEFINE RangeCheck} {$ENDIF}
          ScanEnd1 := PByteArray(Scan)[BestLen - 1];
          ScanEnd := PByteArray(Scan)[BestLen];
          {$IFDEF RangeCheck} {$R+} {$UNDEF RangeCheck} {$ENDIF}
        End;
      End;
    End;
    CurrentMatch := Previous[CurrentMatch And WMask];
    Dec(ChainLength);
  Until (CurrentMatch <= Limit) Or (ChainLength = 0);

  If BestLen <= S.Lookahead Then Result := BestLen
                            Else Result := S.Lookahead;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure FillWindow(Var S: TDeflateState);

// Fills the window when the lookahead becomes insufficient, updates StringStart and Lookahead.
// Lookahead must be less than MIN_LOOKAHEAD.
// StringStart will be <= CurrentWindowSize - MIN_LOOKAHEAD on exit.
// On exit at least one byte has been read, or AvailableInput = 0. Reads are performed for at least two bytes (required
// for the zip translate_eol option -> not supported here). 

Var
  N, M: Cardinal;
  P: PWord;
  More: Cardinal; // amount of free space at the end of the window 
  WSize: Cardinal;

Begin
  WSize := S.WindowSize;
  Repeat
    More := S.CurrentWindowSize - Integer(S.Lookahead) - Integer(S.StringStart);
    If (More = 0) And (S.StringStart = 0) And (S.Lookahead = 0) Then More := WSize
                                                                Else
    If More = Cardinal(-1) Then
    Begin
      // Very unlikely, but sometimes possible if StringStart = 0 and Lookahead = 1 (input done one byte at time)
      Dec(More);
      // If the Window is almost full and there is insufficient lookahead,
      // move the upper half to the lower one to make room in the upper half.
    End
    Else
      If S.StringStart >= WSize + (WSize - MIN_LOOKAHEAD) Then
      Begin
        Move(S.Window[WSize], S.Window^, WSize);
        Dec(S.MatchStart, WSize);
        Dec(S.StringStart, WSize);
        // we now have StringStart >= MaxDistance
        Dec(S.BlockStart, Integer(WSize));

        // Slide the hash table (could be avoided with 32 bit values at the expense of memory usage). We slide even when
        // Level = 0 to keep the hash table consistent if we switch back to Level > 0 later. (Using Level 0 permanently
        // is not an optimal usage of zlib, so we don't care about this pathological case.)
        N := S.HashSize;
        P := @S.Head[N];
        Repeat
          Dec(P);
          M := P^;
          If M >= WSize Then P^ := M - WSize
                        Else P^ := ZNIL;
          Dec(N);
        Until N = 0;

        N := WSize;
        P := @S.Previous[N];
        Repeat
          Dec(P);
          M := P^;
          If M >= WSize Then P^ := M - WSize
                        Else P^ := ZNIL;
          // if N is not on any hash chain Previous[N] is garbage but its value will never be used
          Dec(N);
        Until N = 0;

        Inc(More, WSize);
      End;


    If S.ZState.AvailableInput = 0 Then Exit;

    // If there was no sliding:
    //    StringStart <= WSize + MaxDistance - 1 and Lookahead <= MIN_LOOKAHEAD - 1 and
    //    More = CurrentWindowSize - Lookahead - StringStart
    // => More >= CurrentWindowSize - (MIN_LOOKAHEAD - 1 + WSize + MaxDistance - 1)
    // => More >= CurrentWindowSize - 2 * WSize + 2
    // In the BIG_MEM or MMAP case (not yet supported),
    //    CurrentWindowSize = input_size + MIN_LOOKAHEAD  and
    //    StringStart + S.Lookahead <= input_size => More >= MIN_LOOKAHEAD.
    // Otherwise, CurrentWindowSize = 2 * WSize so More >= 2.
    // If there was sliding More >= WSize. So in all cases More >= 2.

    N := ReadBuffer(S.ZState, @S.Window[S.StringStart + S.Lookahead], More);
    Inc(S.Lookahead, N);

    // Initialize the hash Value now that we have some input:
    If S.Lookahead >= MIN_MATCH Then
    Begin
      S.InsertHash := S.Window[S.StringStart];
      S.InsertHash := ((S.InsertHash Shl S.HashShift) Xor S.Window[S.StringStart + 1]) And S.HashMask;
    End;
    // If the whole input has less than MIN_MATCH bytes, InsertHash is garbage,
    // but this is not important since only literal bytes will be emitted.
  Until (S.Lookahead >= MIN_LOOKAHEAD) Or (S.ZState.AvailableInput = 0);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure FlushBlockOnly(Var S: TDeflateState; EOF: Boolean);

// Flushs the current block with given end-of-file flag.
// StringStart must be set to the end of the current match.

Begin
  If S.BlockStart >= 0 Then TreeFlushBlock(S, @S.Window[Cardinal(S.BlockStart)], Integer(S.StringStart) - S.BlockStart, EOF)
                       Else TreeFlushBlock(S, Nil, Integer(S.StringStart) - S.BlockStart, EOF);

  S.BlockStart := S.StringStart;
  FlushPending(S.ZState^);
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateStored(Var S: TDeflateState; Flush: Integer): TBlockState;

// Copies without compression as much as possible from the input stream and returns the current block state.
// This function does not insert new strings in the dictionary since uncompressible data is probably not useful.
// This function is used only for the Level = 0 compression option.
// NOTE: This function should be optimized to avoid extra copying from Window to PendingBuffer.
//
// Stored blocks are limited to $FFFF bytes, PendingBuffer is limited to PendingBufferSize
// and each stored block has a 5 Byte header.

Var
  MaxBlockSize: Integer;
  MaxStart: Cardinal;

Begin
  MaxBlockSize := $FFFF;
  If MaxBlockSize > S.PendingBufferSize - 5 Then MaxBlockSize := S.PendingBufferSize - 5;

  // copy as much as possible from input to output
  While True Do
  Begin
    // fill the window as much as possible
    If S.Lookahead <= 1 Then
    Begin
      FillWindow(S);
      If (S.Lookahead = 0) And (Flush = Z_NO_FLUSH) Then
      Begin
        Result := bsNeedMore;
        Exit;
      End;

      // flush the current block
      If S.Lookahead = 0 Then Break;
    End;
    Inc(S.StringStart, S.Lookahead);
    S.Lookahead := 0;

    // emit a stored block if PendingBuffer will be full
    MaxStart := S.BlockStart + MaxBlockSize;
    If (S.StringStart = 0) Or (S.StringStart >= MaxStart) Then
    Begin
      // StringStart = 0 is possible when wrap around on 16-bit machine 
      S.Lookahead := S.StringStart - MaxStart;
      S.StringStart := MaxStart;
      FlushBlockOnly(S, False);
      If S.ZState.AvailableOutput = 0 Then
      Begin
        Result := bsNeedMore;
        Exit;
      End;
    End;

    // Flush if we may have to slide, otherwise BlockStart may become negative and the data will be gone.
    If S.StringStart - Cardinal(S.BlockStart) >= S.WindowSize - MIN_LOOKAHEAD Then
    Begin
      FlushBlockOnly(S, False);
      If S.ZState.AvailableOutput = 0 Then
      Begin
        Result := bsNeedMore;
        Exit;
      End;
    End;
  End;

  FlushBlockOnly(S, Flush = Z_FINISH);
  If S.ZState.AvailableOutput = 0 Then
  Begin
    If Flush = Z_FINISH Then Result := bsFinishStarted
                        Else DeflateStored := bsNeedMore;
    Exit;
  End;

  If Flush = Z_FINISH Then Result := bsFinishDone
                      Else Result := bsBlockDone;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateFast(Var S: TDeflateState; Flush: Integer): TBlockState;

// Compresses as much as possible from the input stream and returns the current block state.
// This function does not perform lazy evaluation of matches and inserts new strings in the Dictionary only for
// unmatched strings or for short matches. It is used only for the fast compression options. 

Var
  HashHead: Cardinal;  // head of the hash chain
  BlockFlush: Boolean; // set if current block must be flushed

Begin
  HashHead := ZNIL;
  While True Do
  Begin
    // Make sure that we always have enough lookahead, except at the end of the input file. We need MAX_MATCH bytes
    // for the next match plus MIN_MATCH bytes to insert the string following the next match.
    If S.Lookahead < MIN_LOOKAHEAD Then
    Begin
      FillWindow(S);
      If (S.Lookahead < MIN_LOOKAHEAD) And (Flush = Z_NO_FLUSH) Then
      Begin
        Result := bsNeedMore;
        Exit;
      End;

      // flush the current block
      If S.Lookahead = 0 Then Break;
    End;

    // Insert the string Window[StringStart .. StringStart + 2] in the
    // dictionary and set HashHead to the head of the hash chain.
    If S.Lookahead >= MIN_MATCH Then InsertString(S, S.StringStart, HashHead);

    // Find the longest match, discarding those <= PreviousLength.
    // At this point we have always MatchLength < MIN_MATCH.
    If (HashHead <> ZNIL) And
       (S.StringStart - HashHead <= (S.WindowSize - MIN_LOOKAHEAD)) Then
    Begin
      // To simplify the code, we prevent matches with the string of window index 0 (in particular we have to
      // avoid a match of the string with itself at the start of the input file).
      If S.Strategy <> Z_HUFFMAN_ONLY Then S.MatchLength := LongestMatch(S, HashHead);
    End;
    If S.MatchLength >= MIN_MATCH Then
    Begin
      BlockFlush := TreeTally(S, S.StringStart - S.MatchStart, S.MatchLength - MIN_MATCH);
      Dec(S.Lookahead, S.MatchLength);

      // Insert new strings in the hash table only if the match length
      // is not too large. This saves time but degrades compression.
      If (S.MatchLength <= S.MaxInsertLength) And (S.Lookahead >= MIN_MATCH) Then
      Begin
        // string at StringStart already in hash table
        Dec(S.MatchLength);
        Repeat
          Inc(S.StringStart);
          InsertString(S, S.StringStart, HashHead);
          // StringStart never exceeds WSize - MAX_MATCH, so there are always MIN_MATCH bytes ahead.
          Dec(S.MatchLength);
        Until S.MatchLength = 0;
        Inc(S.StringStart);
      End
      Else
      Begin
        Inc(S.StringStart, S.MatchLength);
        S.MatchLength := 0;
        S.InsertHash := S.Window[S.StringStart];
        S.InsertHash := ((S.InsertHash Shl S.HashShift) Xor S.Window[S.StringStart + 1]) And S.HashMask;

        // if Lookahead < MIN_MATCH, InsertHash is garbage, but it does not
        // matter since it will be recomputed at next Deflate call.
      End;
    End
    Else
    Begin
      // no match, output a literal byte 
      BlockFlush := TreeTally(S, 0, S.Window[S.StringStart]);
      Dec(S.Lookahead);
      Inc(S.StringStart);
    End;

    If BlockFlush Then
    Begin
      FlushBlockOnly(S, False);
      If S.ZState.AvailableOutput = 0 Then
      Begin
        Result := bsNeedMore;
        Exit;
      End;
    End;
  End;

  FlushBlockOnly(S, Flush = Z_FINISH);
  If S.ZState.AvailableOutput = 0 Then
  Begin
    If Flush = Z_FINISH Then Result := bsFinishStarted
                        Else Result := bsNeedMore;
  End
  Else
    If Flush = Z_FINISH Then Result := bsFinishDone
                        Else Result := bsBlockDone;
End;

//----------------------------------------------------------------------------------------------------------------------

Function DeflateSlow(Var S: TDeflateState; Flush: Integer): TBlockState;

// Same as above, but achieves better compression. We use a lazy evaluation for matches. A match is finally adopted
// only if there is no better match at the next window position.

Var
  HashHead: Cardinal;  // head of hash chain
  BlockFlush: Boolean; // set if current block must be flushed
  MaxInsert: Cardinal;

Begin
  HashHead := ZNIL;

  While True Do
  Begin
    // Make sure that we always have enough lookahead, except at the end of the input file. We need MAX_MATCH bytes
    // for the next match, plus MIN_MATCH bytes to insert the string following the next match.
    If S.Lookahead < MIN_LOOKAHEAD Then
    Begin
      FillWindow(S);
      If (S.Lookahead < MIN_LOOKAHEAD) And (Flush = Z_NO_FLUSH) Then
      Begin
        Result := bsNeedMore;
        Exit;
      End;

      // flush the current block
      If S.Lookahead = 0 Then Break;
    End;

    // Insert the string Window[StringStart .. StringStart + 2] in the
    // dictionary and set HashHead to the head of the hash chain.
    If S.Lookahead >= MIN_MATCH Then InsertString(S, S.StringStart, HashHead);

    // find the longest match, discarding those <= PreviousLength
    S.PreviousLength := S.MatchLength;
    S.PreviousMatch := S.MatchStart;
    S.MatchLength := MIN_MATCH - 1;

    If (HashHead <> ZNIL) And
       (S.PreviousLength < S.MaxLazyMatch) And
       (S.StringStart - HashHead <= (S.WindowSize - MIN_LOOKAHEAD)) Then
    Begin
      // To simplify the code we prevent matches with the string of window Index 0 (in particular we have
      // to avoid a match of the string with itself at the start of the input file).
      If S.Strategy <> Z_HUFFMAN_ONLY Then S.MatchLength := LongestMatch(S, HashHead);
      If (S.MatchLength <= 5) And
         ((S.Strategy = Z_FILTERED) Or ((S.MatchLength = MIN_MATCH) And
         (S.StringStart - S.MatchStart > TOO_FAR))) Then
      Begin
        // If PreviousMatch is also MIN_MATCH MatchStart is garbage but we will ignore the current match anyway.
        S.MatchLength := MIN_MATCH - 1;
      End;
    End;

    // If there was a match at the previous step and the current match is not better output the previous match.
    If (S.PreviousLength >= MIN_MATCH) And (S.MatchLength <= S.PreviousLength) Then
    Begin
      MaxInsert := S.StringStart + S.Lookahead - MIN_MATCH;
      // Do not insert strings in hash table beyond this.
      BlockFlush := TreeTally(S, S.StringStart - 1 - S.PreviousMatch, S.PreviousLength - MIN_MATCH);

      // Insert in hash table all strings up to the end of the match. StringStart - 1 and StringStart are already inserted.
      // If there is not enough lookahead the last two strings are not inserted in the hash table.
      Dec(S.Lookahead, S.PreviousLength - 1);
      Dec(S.PreviousLength, 2);
      Repeat
        Inc(S.StringStart);
        If S.StringStart <= MaxInsert Then InsertString(S, S.StringStart, HashHead);
        Dec(S.PreviousLength);
      Until S.PreviousLength = 0;

      S.MatchAvailable := False;
      S.MatchLength := MIN_MATCH - 1;
      Inc(S.StringStart);

      If BlockFlush Then  
      Begin
        FlushBlockOnly(S, False);
        If S.ZState.AvailableOutput = 0 Then
        Begin
          Result := bsNeedMore;
          Exit;
        End;
      End;
    End
    Else
      If S.MatchAvailable Then
      Begin
        // If there was no match at the previous position output a single literal.
        // If there was a match but the current match is longer truncate the previous match to a single literal.
        BlockFlush := TreeTally (S, 0, S.Window[S.StringStart - 1]);
        If BlockFlush Then FlushBlockOnly(S, False);
        Inc(S.StringStart);
        Dec(S.Lookahead);
        If S.ZState.AvailableOutput = 0 Then
        Begin
          Result := bsNeedMore;
          Exit;
        End;
      End
      Else
      Begin
        // There is no previous match to compare with wait for the next step to decide.
        S.MatchAvailable := True;
        Inc(S.StringStart);
        Dec(S.Lookahead);
      End;
  End;

  If S.MatchAvailable Then
  Begin
    TreeTally (S, 0, S.Window[S.StringStart - 1]);
    S.MatchAvailable := False;
  End;

  FlushBlockOnly(S, Flush = Z_FINISH);
  If S.ZState.AvailableOutput = 0 Then
  Begin
    If Flush = Z_FINISH Then Result := bsFinishStarted
                        Else Result := bsNeedMore;
  End
  Else
    If Flush = Z_FINISH Then Result := bsFinishDone
                        Else Result := bsBlockDone;
End;

//----------------- Inflate support ------------------------------------------------------------------------------------

Const
  InflateMask: Array[0..16] Of Cardinal = (
    $0000, $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
    $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF, $FFFF
  );

Function InflateFlush(Var S: TInflateBlocksState; Var Z: TZState; R: Integer): Integer;

// copies as much as possible from the sliding window to the output area

Var
  N: Cardinal;
  P: PByte;
  Q: PByte;

Begin
  // local copies of source and destination pointers
  P := Z.NextOutput;
  Q := S.Read;

  // compute number of bytes to copy as far as end of window 
  If Cardinal(Q) <= Cardinal(S.Write) Then N := Cardinal(S.Write) - Cardinal(Q)
                                      Else N := Cardinal(S.zend) - Cardinal(Q);
  If N > Z.AvailableOutput Then N := Z.AvailableOutput;
  If (N <> 0) And (R = Z_BUF_ERROR) Then R := Z_OK;

  // update counters 
  Dec(Z.AvailableOutput, N);
  Inc(Z.TotalOutput, N);

  // update check information
  If Assigned(S.CheckFunction) Then
  Begin
    S.Check := S.CheckFunction(S.Check, Q, N);
    Z.Adler := S.Check;
  End;

  // copy as far as end of Window 
  Move(Q^, P^, N);
  Inc(P, N);
  Inc(Q, N);

  // see if more to copy at beginning of window 
  If Q = S.zend Then
  Begin
    // wrap pointers
    Q := S.Window;
    If S.Write = S.zend Then S.Write := S.Window;

    // compute bytes to copy 
    N := Cardinal(S.Write) - Cardinal(Q);
    If N > Z.AvailableOutput Then N := Z.AvailableOutput;
    If (N <> 0) And (R = Z_BUF_ERROR) Then R := Z_OK;

    // update counters 
    Dec(Z.AvailableOutput, N);
    Inc(Z.TotalOutput, N);

    // update check information
    If Assigned(S.CheckFunction) Then
    Begin
      S.Check := S.CheckFunction(S.Check, Q, N);
      Z.Adler := S.Check;
    End;

    // copy 
    Move(Q^, P^, N);
    Inc(P, N);
    Inc(Q, N);
  End;

  // update pointers
  Z.NextOutput := P;
  S.Read := Q;

  Result := R;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateFast(LiteralBits, DistanceBits: Cardinal; TL, TD: PInflateHuft; Var S: TInflateBlocksState; Var Z: TZState): Integer;

// Called with number of bytes left to write in window at least 258 (the maximum string length) and number of input
// bytes available at least ten. The ten bytes are six bytes for the longest length/distance pair plus four bytes for
// overloading the bit buffer.

Var
  Temp: PInflateHuft;
  Extra: Cardinal;       // extra bits or operation
  BitsBuffer: Cardinal;
  K: Cardinal;           // bits in bit buffer
  P: PByte;              // input data pointer
  N: Cardinal;           // bytes available there
  Q: PByte;              // output window write pointer
  M: Cardinal;           // bytes to end of window or read pointer
  ml: Cardinal;          // mask for literal/length tree
  md: Cardinal;          // mask for distance tree
  C: Cardinal;           // bytes to copy
  D: Cardinal;           // distance back to copy from
  R: PByte;              // copy source pointer

Begin
  // load input, output, bit values  
  P := Z.NextInput;
  N := Z.AvailableInput;
  BitsBuffer := S.bitb;
  K := S.bitk;
  Q := S.Write;
  If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                    Else M := Cardinal(S.zend)-Cardinal(Q);

  // initialize masks 
  ml := InflateMask[LiteralBits];
  md := InflateMask[DistanceBits];

  // do until not enough input or output space for fast loop,
  // assume called with (M >= 258) and (N >= 10)
  Repeat
    // get literal/length Code
    While K < 20 Do
    Begin
      Dec(N);
      BitsBuffer := BitsBuffer Or (Cardinal(P^) Shl K);
      Inc(P);
      Inc(K, 8);
    End;

    Temp := @PHuftField(TL)[BitsBuffer And ml];

    Extra := Temp.exop;
    If Extra = 0 Then
    Begin
      BitsBuffer := BitsBuffer Shr Temp.Bits;
      Dec(K, Temp.Bits);
      Q^ := Temp.Base;
      Inc(Q);
      Dec(M);
      Continue;
    End;

    Repeat
      BitsBuffer := BitsBuffer Shr Temp.Bits;
      Dec(K, Temp.Bits);

      If (Extra And 16) <> 0 Then
      Begin
        // get extra bits for length 
        Extra := Extra And 15;
        C := Temp.Base + (BitsBuffer And InflateMask[Extra]);
        BitsBuffer := BitsBuffer Shr Extra;
        Dec(K, Extra);
        // decode distance base of block to copy 
        While K < 15 Do
        Begin
          Dec(N);
          BitsBuffer := BitsBuffer Or (Cardinal(P^) Shl K);
          Inc(P);
          Inc(K, 8);
        End;

        Temp := @PHuftField(TD)[BitsBuffer And md];
        Extra := Temp.exop;
        Repeat
          BitsBuffer := BitsBuffer Shr Temp.Bits;
          Dec(K, Temp.Bits);

          If (Extra And 16) <> 0 Then
          Begin
            // get extra bits to add to distance base 
            Extra := Extra And 15;
            While K < Extra Do
            Begin
              Dec(N);
              BitsBuffer := BitsBuffer Or (Cardinal(P^) Shl K);
              Inc(P);
              Inc(K, 8);
            End;

            D := Temp.Base + (BitsBuffer And InflateMask[Extra]);
            BitsBuffer := BitsBuffer Shr Extra;
            Dec(K, Extra);

            // do the copy 
            Dec(M, C);
            // offset before Dest
            If (Cardinal(Q) - Cardinal(S.Window)) >= D Then
            Begin
              //  just copy 
              R := Q;
              Dec(R, D);
              Q^ := R^;  Inc(Q); Inc(R); Dec(C); // minimum count is three,
              Q^ := R^;  Inc(Q); Inc(R); Dec(C); // so unroll loop a little
            End
            Else
            Begin
              // offset after destination,
              // bytes from offset to end
              Extra := D - (Cardinal(Q) - Cardinal(S.Window));
              R := S.zend;
              // pointer to offset
              Dec(R, Extra);
              If C > Extra Then
              Begin
                // copy to end of window
                Dec(C, Extra);
                Repeat
                  Q^ := R^;
                  Inc(Q);
                  Inc(R);
                  Dec(Extra);
                Until Extra = 0;
                // copy rest from start of window
                R := S.Window;
              End;
            End;

            // copy all or what's left
            Repeat
              Q^ := R^;
              Inc(Q);
              Inc(R);
              Dec(C);
            Until C = 0;
            Break;
          End
          Else
            If (Extra And 64) = 0 Then
            Begin
              Inc(Temp, Temp.Base + (BitsBuffer And InflateMask[Extra]));
              Extra := Temp.exop;
            End
          Else
          Begin
            Z.Msg := SInvalidDistanceCode;
            C := Z.AvailableInput - N;
            If (K Shr 3) < C Then C := K Shr 3;
            Inc(N, C);
            Dec(P, C);
            Dec(K, C Shl 3);
            S.bitb := BitsBuffer;
            S.bitk := K;
            Z.AvailableInput := N;
            Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
            Z.NextInput := P;
            S.Write := Q;
            Result := Z_DATA_ERROR;
            Exit;
          End;
        Until False;
        Break;
      End;

      If (Extra And 64) = 0 Then
      Begin
        Inc(Temp, Temp.Base + (BitsBuffer And InflateMask[Extra]));
        Extra := Temp.exop;
        If Extra = 0 Then
        Begin
          BitsBuffer := BitsBuffer Shr Temp.Bits;
          Dec(K, Temp.Bits);

          Q^ := Temp.Base;
          Inc(Q);
          Dec(M);
          Break;
        End;
      End
      Else
        If (Extra And 32) <> 0 Then
        Begin
          C := Z.AvailableInput - N;
          If (K Shr 3) < C Then C := K Shr 3;
          Inc(N, C);
          Dec(P, C);
          Dec(K, C Shl 3);
          S.bitb := BitsBuffer;
          S.bitk := K;
          Z.AvailableInput := N;
          Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
          Z.NextInput := P;
          S.Write := Q;
          Result := Z_STREAM_END;
          Exit;
        End
        Else
        Begin
          Z.Msg := SInvalidLengthCode;
          C := Z.AvailableInput - N;
          If (K Shr 3) < C Then C := K Shr 3;
          Inc(N, C);
          Dec(P, C);
          Dec(K, C Shl 3);
          S.bitb := BitsBuffer;
          S.bitk := K;
          Z.AvailableInput := N;
          Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
          Z.NextInput := P;
          S.Write := Q;
          Result := Z_DATA_ERROR;
          Exit;
        End;
    Until False;
  Until (M < 258) Or (N < 10);

  // not enough input or output -> restore pointers and return 
  C := Z.AvailableInput - N;
  If (K Shr 3) < C Then C := K Shr 3;
  Inc(N, C);
  Dec(P, C);
  Dec(K, C Shl 3);
  S.bitb := BitsBuffer;
  S.bitk := K;
  Z.AvailableInput := N;
  Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
  Z.NextInput := P;
  S.Write := Q;
  Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateCodesNew(LiteralBits: Cardinal; DistanceBits: Cardinal; TL, TD: PInflateHuft;
  Var Z: TZState): PInflateCodesState;

Begin
  Result := AllocMem(SizeOf(TInflateCodesState));
  Result.Mode := icmStart;
  Result.LiteralTreeBits := LiteralBits;
  Result.DistanceTreeBits := DistanceBits;
  Result.LiteralTree := TL;
  Result.DistanceTree := TD;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateCodes(Var S: TInflateBlocksState; Var Z: TZState; R: Integer): Integer;

Var
  J: Cardinal;          // temporary storage
  Temp: PInflateHuft;
  Extra: Cardinal;      // extra bits or operation 
  BitsBuffer: Cardinal;
  K: Cardinal;          // bits in bit buffer 
  P: PByte;             // input data pointer 
  N: Cardinal;          // bytes available there
  Q: PByte;             // output window write pointer
  M: Cardinal;          // bytes to end of window or read pointer
  F: PByte;             // pointer to copy strings from 
  C: PInflateCodesState;
  
Begin
  C := S.sub.decode.codes;  // codes state 

  // copy input/output information to locals 
  P := Z.NextInput;
  N := Z.AvailableInput;
  BitsBuffer := S.bitb;
  K := S.bitk;
  Q := S.Write;
  If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                    Else M := Cardinal(S.zend)-Cardinal(Q);

  // process input and output based on current state
  While True Do
  Begin
    Case C.Mode Of
      icmStart:
        Begin
          If (M >= 258) And (N >= 10) Then
          Begin
            S.bitb := BitsBuffer;
            S.bitk := K;
            Z.AvailableInput := N;
            Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
            Z.NextInput := P;
            S.Write := Q;

            R := InflateFast(C.LiteralTreeBits, C.DistanceTreeBits, C.LiteralTree, C.DistanceTree, S, Z);
            P := Z.NextInput;
            N := Z.AvailableInput;
            BitsBuffer := S.bitb;
            K := S.bitk;
            Q := S.Write;
            If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                              Else M := Cardinal(S.zend) - Cardinal(Q);

            If R <> Z_OK Then
            Begin
              If R = Z_STREAM_END Then C.mode := icmWash
                                  Else C.mode := icmBadCode;
              Continue;    
            End;
          End;
          C.sub.Code.need := C.LiteralTreeBits;
          C.sub.Code.Tree := C.LiteralTree;
          C.mode := icmLen;   
        End;
      icmLen: // I: get length/literal/eob next
        Begin
          J := C.sub.Code.need;
          While K < J Do
          Begin
            If N <> 0 Then R := Z_OK
                      Else
            Begin
              S.bitb := BitsBuffer;
              S.bitk := K;
              Z.AvailableInput := N;
              Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
              Z.NextInput := P;
              S.Write := Q;
              Result := InflateFlush(S, Z, R);
              Exit;
            End;
            Dec(N);
            BitsBuffer := BitsBuffer Or (Cardinal(P^) Shl K);
            Inc(P);
            Inc(K, 8);
          End;
          Temp := C.sub.Code.Tree;
          Inc(Temp, Cardinal(BitsBuffer) And InflateMask[J]);
          BitsBuffer := BitsBuffer Shr Temp.Bits;
          Dec(K, Temp.Bits);

          Extra := Temp.exop;
          // literal
          If Extra = 0 Then
          Begin
            C.sub.lit := Temp.Base;
            C.mode := icmLit;
            Continue;   
          End;
          // length
          If (Extra And 16) <> 0 Then
          Begin
            C.sub.Copy.get := Extra And 15;
            C.Len := Temp.Base;
            C.mode := icmLenNext;
            Continue;
          End;
          // next table
          If (Extra And 64) = 0 Then
          Begin
            C.sub.Code.need := Extra;
            C.sub.Code.Tree := @PHuftField(Temp)[Temp.Base];
            Continue;          
          End;
          // end of block
          If (Extra And 32) <> 0 Then
          Begin
            C.mode := icmWash;
            Continue;          
          End;
          // invalid code
          C.mode := icmBadCode;
          Z.Msg := SInvalidLengthCode;
          R := Z_DATA_ERROR;
          S.bitb := BitsBuffer;
          S.bitk := K;
          Z.AvailableInput := N;
          Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
          Z.NextInput := P;
          S.Write := Q;
          Result := InflateFlush(S, Z, R);
          Exit;
        End;
      icmLenNext: // I: getting length extra (have base)
        Begin
          J := C.sub.Copy.get;
          While K < J Do
          Begin
            If N <> 0 Then R := Z_OK
                      Else
            Begin
              S.bitb := BitsBuffer;
              S.bitk := K;
              Z.AvailableInput := N;
              Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
              Z.NextInput := P;
              S.Write := Q;
              Result := InflateFlush(S, Z, R);
              Exit;
            End;
            Dec(N);
            BitsBuffer := BitsBuffer Or (Cardinal(P^) Shl K);
            Inc(P);
            Inc(K, 8);
          End;
          Inc(C.Len, Cardinal(BitsBuffer And InflateMask[J]));
          BitsBuffer := BitsBuffer Shr J;
          Dec(K, J);

          C.sub.Code.need := C.DistanceTreeBits;
          C.sub.Code.Tree := C.DistanceTree;
          C.mode := icmDistance;
        End;
      icmDistance: // I: get distance next 
        Begin
          J := C.sub.Code.need;
          While K < J Do
          Begin
            If N <> 0 Then R := Z_OK
                      Else
            Begin
              S.bitb := BitsBuffer;
              S.bitk := K;
              Z.AvailableInput := N;
              Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
              Z.NextInput := P;
              S.Write := Q;
              Result := InflateFlush(S, Z, R);
              Exit;
            End;
            Dec(N);
            BitsBuffer := BitsBuffer Or (Cardinal(P^) Shl K);
            Inc(P);
            Inc(K, 8);
          End;
          Temp := @PHuftField(C.sub.Code.Tree)[BitsBuffer And InflateMask[J]];
          BitsBuffer := BitsBuffer Shr Temp.Bits;
          Dec(K, Temp.Bits);

          Extra := Temp.exop;
          // distance
          If (Extra And 16) <> 0 Then
          Begin
            C.sub.Copy.get := Extra And 15;
            C.sub.Copy.Distance := Temp.Base;
            C.mode := icmDistExt;
            Continue;
          End;
          // next table
          If (Extra And 64) = 0 Then
          Begin
            C.sub.Code.need := Extra;
            C.sub.Code.Tree := @PHuftField(Temp)[Temp.Base];
            Continue;      
          End;
          // invalid code
          C.mode := icmBadCode;
          Z.Msg := SInvalidDistanceCode;
          R := Z_DATA_ERROR;
          S.bitb := BitsBuffer;
          S.bitk := K;
          Z.AvailableInput := N;
          Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
          Z.NextInput := P;
          S.Write := Q;
          Result := InflateFlush(S, Z, R);
          Exit;
        End;
      icmDistExt: // I: getting distance extra
        Begin
          J := C.sub.Copy.get;
          While K < J Do
          Begin
            If N <> 0 Then R := Z_OK
                      Else
            Begin
              S.bitb := BitsBuffer;
              S.bitk := K;
              Z.AvailableInput := N;
              Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
              Z.NextInput := P;
              S.Write := Q;
              Result := InflateFlush(S, Z, R);
              Exit;
            End;
            Dec(N);
            BitsBuffer := BitsBuffer Or (Cardinal(P^) Shl K);
            Inc(P);
            Inc(K, 8);
          End;
          Inc(C.sub.Copy.Distance, Cardinal(BitsBuffer) And InflateMask[J]);
          BitsBuffer := BitsBuffer Shr J;
          Dec(K, J);
          C.mode := icmCopy;
        End;
      icmCopy: // O: copying bytes in window, waiting for space
        Begin
          F := Q;
          Dec(F, C.sub.Copy.Distance);
          If (Cardinal(Q) - Cardinal(S.Window)) < C.sub.Copy.Distance Then
          Begin
            F := S.zend;
            Dec(F, C.sub.Copy.Distance - (Cardinal(Q) - Cardinal(S.Window)));
          End;

          While C.Len <> 0 Do
          Begin
            If M = 0 Then
            Begin
              If (Q = S.zend) And (S.Read <> S.Window) Then
              Begin
                Q := S.Window;
                If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                  Else M := Cardinal(S.zend)-Cardinal(Q);
              End;

              If M = 0 Then
              Begin
                S.Write := Q;
                R := InflateFlush(S, Z, R);
                Q := S.Write;
                If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                  Else M := Cardinal(S.zend) - Cardinal(Q);

                If (Q = S.zend) And (S.Read <> S.Window) Then
                Begin
                  Q := S.Window;
                  If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                    Else M := Cardinal(S.zend) - Cardinal(Q);
                End;

                If M = 0 Then
                Begin
                  S.bitb := BitsBuffer;
                  S.bitk := K;
                  Z.AvailableInput := N;
                  Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
                  Z.NextInput := P;
                  S.Write := Q;
                  Result := InflateFlush(S, Z, R);
                  Exit;
                End;
              End;
            End;
            R := Z_OK;

            Q^ := F^;
            Inc(Q);
            Inc(F);
            Dec(M);

            If (F = S.zend) Then F := S.Window;
            Dec(C.Len);
          End;
          C.mode := icmStart;
        End;
      icmLit: // O: got literal, waiting for output space
        Begin
          If M = 0 Then
          Begin
            If (Q = S.zend) And (S.Read <> S.Window) Then
            Begin
              Q := S.Window;
              If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                Else M := Cardinal(S.zend) - Cardinal(Q);
            End;

            If M = 0 Then
            Begin
              S.Write := Q;
              R := InflateFlush(S, Z, R);
              Q := S.Write;
              If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                Else M := Cardinal(S.zend) - Cardinal(Q);

              If (Q = S.zend) And (S.Read <> S.Window) Then
              Begin
                Q := S.Window;
                If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                  Else M := Cardinal(S.zend) - Cardinal(Q);
              End;

              If M = 0 Then
              Begin
                S.bitb := BitsBuffer;
                S.bitk := K;
                Z.AvailableInput := N;
                Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
                Z.NextInput := P;
                S.Write := Q;
                Result := InflateFlush(S, Z, R);
                Exit;
              End;
            End;
          End;
          R := Z_OK;
          Q^ := C.sub.lit;
          Inc(Q);
          Dec(M);
          C.mode := icmStart;
        End;
      icmWash: // O: got eob, possibly More output
        Begin
          // return unused byte, if any
          If K > 7 Then
          Begin
            Dec(K, 8);
            Inc(N);
            Dec(P);
            // can always return one
          End;
          S.Write := Q;
          R := InflateFlush(S, Z, R);
          Q := S.Write;
          If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                            Else M := Cardinal(S.zend) - Cardinal(Q);

          If S.Read <> S.Write Then
          Begin
            S.bitb := BitsBuffer;
            S.bitk := K;
            Z.AvailableInput := N;
            Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
            Z.NextInput := P;
            S.Write := Q;
            Result := InflateFlush(S, Z, R);
            Exit;
          End;
          C.mode := icmZEnd;
        End;
      icmZEnd:
        Begin
          R := Z_STREAM_END;
          S.bitb := BitsBuffer;
          S.bitk := K;
          Z.AvailableInput := N;
          Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
          Z.NextInput := P;
          S.Write := Q;
          Result := InflateFlush(S, Z, R);
          Exit;
        End;
      icmBadCode: // X: got error
        Begin
          R := Z_DATA_ERROR;
          S.bitb := BitsBuffer;
          S.bitk := K;
          Z.AvailableInput := N;
          Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
          Z.NextInput := P;
          S.Write := Q;
          Result := InflateFlush(S, Z, R);
          Exit;
        End;
    Else
      Begin
        R := Z_STREAM_ERROR;
        S.bitb := BitsBuffer;
        S.bitk := K;
        Z.AvailableInput := N;
        Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
        Z.NextInput := P;
        S.Write := Q;
        Result := InflateFlush(S, Z, R);
        Exit;
      End;
    End;
  End;
  
  Result := Z_STREAM_ERROR;
End;

//----------------------------------------------------------------------------------------------------------------------

Const
  // Maximum Size of dynamic tree. The maximum found in an integer but non-exhaustive search was 1004 huft structures
  // (850 for length/literals and 154 for distances, the latter actually the result of an exhaustive search).
  // The actual maximum is not known, but the value below is more than safe. 
  MANY = 1440;

  // Tables for deflate from PKZIP'S appnote.txt
  // copy lengths for literal codes 257..285 (actually lengths - 2; also see note #13 above about 258)
  CopyLengths: Array [0..30] Of Cardinal = (
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35,
    43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0
  );

  INVALID_CODE = 112;
  // extra bits for literal codes 257..285
  CopyLiteralExtra: Array [0..30] Of Cardinal = (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, INVALID_CODE, INVALID_CODE
  );

  // copy offsets for distance codes 0..29
  CopyOffsets: Array [0..29] Of Cardinal = (
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385,
    513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
  );

  // extra bits for distance codes
  CopyExtra: Array [0..29] Of Cardinal = (
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7,
    7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13
  );

  // Huffman code decoding is performed using a multi-Level table lookup.
  // Fastest way to decode is to simply build a lookup table whose
  // size is determined by the longest code. However, the time it takes
  // to build this table can also be a factor if the data being decoded
  // is not very integer. The most common codes are necessarily the
  // shortest codes so those codes dominate the decoding time and hence
  // the speed. The idea is you can have a shorter table that decodes the
  // shorter, More probable codes, and then point to subsidiary tables for
  // the longer codes. The time it costs to decode the longer codes is
  // then traded against the time it takes to make longer tables.
  //
  // This results of this trade are in the variables LiteralTreeBits and DistanceTreeBits
  // below. LiteralTreeBits is the number of bits the first level table for literal/
  // length codes can decode in one step, and DistanceTreeBits is the same thing for
  // the distance codes. Subsequent tables are also less than or equal to those sizes.
  // These values may be adjusted either when all of the
  // codes are shorter than that, in which case the longest code length in
  // bits is used, or when the shortest code is *longer* than the requested
  // table size, in which case the length of the shortest code in bits is used.
  //
  // There are two different values for the two tables, since they code a
  // different number of possibilities each. The literal/length table
  // codes 286 possible values, or in a flat code, a little over eight
  // bits. The distance table codes 30 possible values, or a little less
  // than five bits, flat. The optimum values for speed end up being
  // about one bit more than those, so LiteralTreeBits is 8 + 1 and DistanceTreeBits is 5 + 1.
  // The optimum values may differ though from machine to machine, and possibly even between compilers. 

Const
  // maximum bit length of any code,
  // If BMAX needs to be larger than 16, then H and X[] should be Cardinal.
  BMAX = 15;

//----------------------------------------------------------------------------------------------------------------------

Function BuildHuffmanTables(Const B: Array Of Cardinal; N, S: Cardinal; Const D, Extra: Array Of Cardinal;
  Temp: PPInflateHuft; Var M: Cardinal; Var HP: Array Of TInflateHuft; Var HN: Cardinal;
  Var V: Array Of Cardinal): Integer;

// Given a list of code lengths and a maximum table size, make a set of tables to decode that set of codes. Returns Z_OK
// on success, Z_BUF_ERROR if the given code set is incomplete (the tables are still built in this case), Z_DATA_ERROR
// if the input is invalid (an over-subscribed set of lengths), or Z_MEM_ERROR if not enough memory.
//
// Input pareters:
// B contains the code lenths in bits (all assumed <= BMAX)
// N is the number of codes (<= NMAX)
// S is the number of simple valued codes (0..S - 1)
// D contains a list of base values for non-simple codes
// Extra carries a list of extra bits for non-simple codes
//
// Output parameters:
// Temp points to the starting table
// M receives the maxium lookup bits (actual space for trees)
// HP receives the Huffman tables
// while HN decribes how many of HP is actually used
// finally V is a working area which receives values in order of bit length

Var
  A: Cardinal;                     // counter for codes of length K 
  C: Array [0..BMAX] Of Cardinal;  // bit length count table
  F: Cardinal;                     // I repeats in table every F entries
  G: Integer;                      // maximum code Length
  H: Integer;                      // table Level
  I: Cardinal;                     // counter, current code
  J: Cardinal;                     // counter
  K: Integer;                      // number of bits in current code
  L: Integer;                       // bits per table (returned in M)
  Mask: Cardinal;                  // (1 shl W) - 1, to avoid cc - O bug on HP
  P: PCardinal;                    // pointer into C[], B[], or V[]
  Q: PInflateHuft;                 // points to current table
  R: TInflateHuft;                 // table entry for structure assignment
  U: Array [0..BMAX - 1] Of PInflateHuft; // table stack
  W: Integer;                      // bits before this table = (L * H)
  X: Array [0..BMAX] Of Cardinal;  // bit offsets, then code stack 
  XP: PCardinal;                   // pointer into X 
  Y: Integer;                      // number of dummy codes added 
  Z: Cardinal;                     // number of entries in current table 
  
Begin
  // generate counts for each bit length 
  FillChar(C, SizeOf(C), 0);    

  // assume all entries <= BMAX
  For I := 0 To N - 1 Do Inc(C[B[I]]);

  // nil input -> all zero length codes
  If C[0] = N Then
  Begin
    Temp^ := Nil;
    M := 0 ;
    Result := Z_OK;
    Exit;
  End ;

  // find minimum and maximum length, bound [M] by those 
  L := M;
  For J := 1 To BMAX Do
    If C[J] <> 0 Then Break;
  // minimum code Length
  K := J ;
  If Cardinal(L) < J Then L := J;
  For I := BMAX DownTo 1 Do
    If C[I] <> 0 Then Break;
  // maximum code length
  G := I ;
  If Cardinal(L) > I Then L := I;
  M := L;

  // adjust last length count to fill out codes if needed
  Y := 1 Shl J;
  While J < I Do
  Begin
    Dec(Y, C[J]);
    If Y < 0 Then
    Begin
      // bad input: more codes than bits
      Result := Z_DATA_ERROR;
      Exit;
    End ;
    Inc(J);
    Y := Y Shl 1;
  End;
  Dec (Y, C[I]);
  If Y < 0 Then
  Begin
    // bad input: more codes than bits
    Result := Z_DATA_ERROR;
    Exit;
  End;
  Inc(C[I], Y);

  // generate starting offsets into the value table for each length
  X[1] := 0;
  J := 0;

  P := @C[1];
  XP := @X[2];
  // note that I = G from above
  Dec(I);
  While (I > 0) Do
  Begin
    Inc(J, P^);
    XP^ := J;
    Inc(P);
    Inc(XP);
    Dec(I);
  End;

  // make a table of values in order of bit lengths
  For I := 0 To N - 1 Do
  Begin
    J := B[I];
    If J <> 0 Then
    Begin
      V[X[J]] := I;
      Inc(X[J]);
    End;
  End;
  // set N to Length of V
  N := X[G];

  // generate the Huffman codes and for each make the table entries
  I := 0;
  // first Huffman code is zero
  X[0] := 0;
  // grab values in bit order
  P := @V;
  // no tables yet -> Level - 1
  H := -1;
  // bits decoded = (L * H)
  W := -L;

  U[0] := Nil;
  Q := Nil;
  Z := 0;        

  // go through the bit lengths (K already is bits in shortest code) 
  While K <= G Do
  Begin
    A := C[K];
    While A <> 0 Do
    Begin
      Dec(A);
      // here I is the Huffman code of length K bits for value P^ 
      // make tables up to required level 
      While K > W + L Do
      Begin
        Inc(H);
        // add bits already decoded, previous table always L Bits
        Inc(W, L);
        // compute minimum size table less than or equal to L bits
        Z := G - W;
        If Z > Cardinal(L) Then Z := L;

        // try a K - W bit table
        J := K - W;
        F := 1 Shl J;
        // too few codes for K - W bit table
        If F > A + 1 Then
        Begin
          // deduct codes from patterns left
          Dec(F,A + 1);
          XP := @C[K];
          If J < Z Then
          Begin
            Inc(J);
            While J < Z Do
            Begin
              // try smaller tables up to Z bits
              F := F Shl 1;
              Inc(XP);
              // enough codes to use up J Bits
              If F <= XP^ Then Break;
              // else deduct codes from patterns
              Dec(F, XP^);
              Inc(J);
            End;
          End;
        End;

        // table entries for J-bit table
        Z := 1 Shl J;
        // allocate new table (note: doesn't matter for fixed)
        If HN + Z > MANY Then
        Begin
          Result := Z_MEM_ERROR;
          Exit;
        End;

        Q := @HP[HN];
        U[H] := Q;
        Inc(HN, Z);

        // connect to last table, if there is one 
        If H <> 0 Then
        Begin
          // save pattern for backing up
          X[H] := I;
          // bits to dump before this table
          R.Bits := L;
          // bits in this table
          R.exop := J;
          J := I Shr (W - L);
          R.Base := (Cardinal(Q) - Cardinal(U[H - 1]) ) Div SizeOf(Q^) - J;
          // connect to last table
          PHuftField(U[H - 1])[J] := R;
        End
        Else
          // first table is returned result
          Temp^ := Q;
      End;

      // set up table entry in R 
      R.Bits := Byte(K - W);

      // out of values -> invalid code
      If Cardinal(P) >= Cardinal(@V[N]) Then R.exop := 128 + 64
                                        Else
        If P^ < S Then
        Begin
          // 256 is end-of-block code
          If P^ < 256 Then R.exop := 0
                      Else R.exop := 32 + 64;
          // simple code is just the value
          R.Base := P^;
          Inc(P);
        End
        Else
        Begin
          // non-simple -> look up in lists
          R.exop := Byte(Extra[P^ - S] + 16 + 64);
          R.Base := D[P^ - S];
          Inc (P);
        End;

      // fill xode-like entries with R
      F := 1 Shl (K - W);
      J := I Shr W;
      While J < Z Do
      Begin
        PHuftField(Q)[J] := R;
        Inc(J, F);
      End;

      // backwards increment the K-bit code I 
      J := 1 Shl (K - 1) ;
      While (I And J) <> 0 Do
      Begin
        I := I Xor J;         
        J := J Shr 1
      End;
      I := I Xor J;

      // backup over finished tables
      // needed on HP, cc -O bug
      Mask := (1 Shl W) - 1;
      While (I And Mask) <> X[H] Do
      Begin
        // don't need to update Q
        Dec(H);
        Dec(W, L);
        Mask := (1 Shl W) - 1;
      End;
    End;
    Inc(K);
  End;

  // Return Z_BUF_ERROR if we were given an incomplete table 
  If (Y <> 0) And (G <> 1) Then Result := Z_BUF_ERROR
                           Else Result := Z_OK;
End; 

//----------------------------------------------------------------------------------------------------------------------

Function InflateTreesBits(Var C: Array Of Cardinal; Var BB: Cardinal; Var TB: PInflateHuft;
  Var HP: Array Of TInflateHuft; Var Z: TZState): Integer;

// C holds 19 code lengths
// BB - bits tree desired/actual depth
// TB - bits tree result
// HP - space for trees
// Z - for messages

Var
  R: Integer;
  HN: Cardinal;          // hufts used in space 
  V: PCardinalArray;     // work area for BuildHuffmanTables 

Begin
  HN := 0;
  V := AllocMem(19 * SizeOf(Cardinal));
  Try
    R := BuildHuffmanTables(C, 19, 19, CopyLengths, CopyLiteralExtra, @TB, BB, HP, HN, V^);
    If R = Z_DATA_ERROR Then Z.Msg := SOversubscribedDBLTree
                        Else
      If (R = Z_BUF_ERROR) Or (BB = 0) Then
      Begin
        Z.Msg := SIncompleteDBLTree;
        R := Z_DATA_ERROR;
      End;
      
    Result := R;
  Finally
    FreeMem(V);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateTreesDynamic(NL: Cardinal; ND: Cardinal; Var C: Array Of Cardinal; Var LiteralBits: Cardinal;
  Var DistanceBits: Cardinal; Var TL: PInflateHuft; Var TD: PInflateHuft; Var HP: Array Of TInflateHuft;
  Var Z: TZState): Integer;

// NL - number of literal/length codes
// ND - number of distance codes
// C - code lengths
// LiteralBits - literal desired/actual bit depth
// DistanceBits - distance desired/actual bit depth
// TL - literal/length tree result
// TD - distance tree result
// HP - space for trees
// Z - for messages

Var
  R: Integer;
  HN: Cardinal;          // hufts used in space
  V: PCardinalArray;     // work area for BuildHuffmanTables

Begin
  HN := 0;
  // allocate work area
  V := AllocMem(288 * SizeOf(Cardinal));
  Try
    Result := Z_OK;

    // build literal/length tree
    R := BuildHuffmanTables(C, NL, 257, CopyLengths, CopyLiteralExtra, @TL, LiteralBits, HP, HN, V^);
    If (R <> Z_OK) Or (LiteralBits = 0) Then
    Begin
      If R = Z_DATA_ERROR Then Z.Msg := SOversubscribedLLTree
                          Else
        If R <> Z_MEM_ERROR Then
        Begin
          Z.Msg := SIncompleteLLTree;
          R := Z_DATA_ERROR;
        End;

      FreeMem(V);
      Result := R;
      Exit;
    End;

    // build distance tree
    R := BuildHuffmanTables(PCardinalArray(@C[NL])^, ND, 0, CopyOffsets, CopyExtra, @TD, DistanceBits, HP, HN, V^);
    If (R <> Z_OK) Or ((DistanceBits = 0) And (NL > 257)) Then
    Begin
      If R = Z_DATA_ERROR Then Z.Msg := SOversubscribedLLTree
                          Else
        If R = Z_BUF_ERROR Then
        Begin
          Z.Msg := SIncompleteLLTree;
          R := Z_DATA_ERROR;
        End
        Else
          If R <> Z_MEM_ERROR Then
          Begin
            Z.Msg := SEmptyDistanceTree;
            R := Z_DATA_ERROR;
          End;
      FreeMem(V);
      Result := R;
    End;
  Finally
    FreeMem(V);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Var
  // build fixed tables only once -> keep them here
  FixedBuild: Boolean = False;

Const
  // number of hufts used by fixed tables
  FIXEDH = 544;
  
Var
  FixedTablesMemory: Array[0..FIXEDH - 1] Of TInflateHuft;
  FixedLiteralBits: Cardinal;
  FixedDistanceBits: Cardinal;
  FixedLiteralTable: PInflateHuft;
  FixedDistanceTable: PInflateHuft;

//----------------------------------------------------------------------------------------------------------------------

Function InflateTreesFixed(Var LiteralBits: Cardinal; Var DistanceBits: Cardinal; Var TL, TD: PInflateHuft;
  Var Z: TZState): Integer;

Type
  PFixedTable = ^TFixedTable;
  TFixedTable = Array[0..287] Of Cardinal;

Var
  K: Integer;        // temporary variable
  C: PFixedTable;    // length list for BuildHuffmanTables
  V: PCardinalArray; // work area for BuildHuffmanTables
  F: Cardinal;       // number of hufts used in FixedTablesMemory

Begin
  // build fixed tables if not already (multiple overlapped executions ok) 
  If Not FixedBuild Then
  Begin
    F := 0;
    C := Nil;
    V := Nil;

    Try
      C := AllocMem(288 * SizeOf(Cardinal));
      V := AllocMem(288 * SizeOf(Cardinal));
      // literal table
      For K := 0 To 143 Do C[K] := 8;
      For K := 144 To 255 Do C[K] := 9;
      For K := 256 To 279 Do C[K] := 7;
      For K := 280 To 287 Do C[K] := 8;
      FixedLiteralBits := 9;
      BuildHuffmanTables(C^, 288, 257, CopyLengths, CopyLiteralExtra, @FixedLiteralTable, FixedLiteralBits,
                         FixedTablesMemory, F, V^);

      // distance table
      For K := 0 To 29 Do C[K] := 5;
      FixedDistanceBits := 5;
      BuildHuffmanTables(C^, 30, 0, CopyOffsets, CopyExtra, @FixedDistanceTable, FixedDistanceBits, FixedTablesMemory,
                         F, V^);

      FixedBuild := True;
    Finally
      If Assigned(V) Then FreeMem(V);
      If Assigned(C) Then FreeMem(C);
    End;
  End;
  LiteralBits := FixedLiteralBits;
  DistanceBits := FixedDistanceBits;
  TL := FixedLiteralTable;
  TD := FixedDistanceTable;
  Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

// tables for Deflate from PKZIP'S appnote.txt.
Const
  // order of the bit length code lengths
  BitOrder: Array [0..18] Of Word = (
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
  );

// Notes beyond the 1.93a appnote.txt:
// 1. Distance pointers never point before the beginning of the output stream.
// 2. Distance pointers can point back across blocks, up to 32k away.
// 3. There is an implied maximum of 7 Bits for the bit Length table and 15 Bits for the actual data.
// 4. if only one Code exists, then it is encoded using one bit. (zero would be more efficient, but perhaps a little
//    confusing.) If two codes exist, they are coded using one bit each (0 and 1).
// 5. There is no way of sending zero distance codes -> a dummy must be sent if there are none. (History: a pre 2.0
//    Version of PKZIP would store blocks with no distance codes, but this was discovered to be
//    too harsh a criterion.) Valid only for 1.93a. 2.04c does allow zero distance codes, which is sent as one Code of
//    zero Bits in length.
// 6. There are up to 286 literal/Length codes. Code 256 represents the end-of-block. Note however that the static
//    length Tree defines 288 codes just to fill out the Huffman codes. Codes 286 and 287 cannot be used though, since
//    there is no length base or extra bits defined for them. Similarily, there are up to 30 distance codes. However,
//    static trees defines 32 codes (all 5 Bits) to fill out the Huffman codes, but the last two had better not show up
//    in the data.
// 7. Unzip can check dynamic Huffman blocks for complete code sets. The exception is that a single code would not be
//    complete (see #4).
// 8. The five Bits following the block type is really the number of literal codes sent minus 257.
// 9. Length codes 8, 16, 16 are interpreted as 13 Length codes of 8 bits (1 + 6 + 6). Therefore, to output three times
//    the length, you output three codes (1 + 1 + 1), whereas to output four times the same length,
//    you only need two codes (1+3).  Hmm.
// 10. In the tree reconstruction algorithm, Code = Code + Increment only if BitLength(I) is not zero (pretty obvious).
// 11. Correction: 4 Bits: # of Bit Length codes - 4 (4 - 19)
// 12. Note: length code 284 can represent 227 - 258, but length code 285 really is 258. The last length deserves its
//     own, short code since it gets used a lot in very redundant files. The length 258 is special since 258 - 3 (the
//     min match length) is 255.
// 13. The literal/length and distance code bit lengths are read as a single stream of lengths.  It is possible (and
//     advantageous) for a repeat code (16, 17, or 18) to go across the boundary between the two sets of lengths.
//----------------------------------------------------------------------------------------------------------------------

Procedure InflateBlockReset(Var S: TInflateBlocksState; Var Z: TZState; C: PCardinal);

Begin
  If Assigned(C) Then C^ := S.Check;
  If (S.mode = ibmBitTree) Or (S.mode = ibmDistTree) Then FreeMem(S.sub.trees.blens);
  If S.mode = ibmCodes Then FreeMem(S.sub.decode.codes);

  S.mode := ibmZType;
  S.bitk := 0;
  S.bitb := 0;

  S.Write := S.Window;
  S.Read := S.Window;
  If Assigned(S.CheckFunction) Then
  Begin
    S.Check := S.CheckFunction(0, Nil, 0);
    Z.Adler := S.Check;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateBlocksNew(Var Z: TZState; C: TCheckFunction; W: Cardinal): PInflateBlocksState;

// W is the window size

Var
  S: PInflateBlocksState;

Begin
  S := AllocMem(SizeOf(TInflateBlocksState));
  If S = Nil Then Result := S
             Else
  Try
    S.hufts := AllocMem(SizeOf(TInflateHuft) * MANY);

    S.Window := AllocMem(W);
    S.zend := S.Window;
    Inc(S.zend, W);
    S.CheckFunction := C;
    S.mode := ibmZType;
    InflateBlockReset(S^, Z, Nil);
    Result := S;
  Except
    If Assigned(S.Window) Then FreeMem(S.Window);
    If Assigned(S.hufts) Then FreeMem(S.hufts);
    FreeMem(S);
    Raise;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateBlocks(Var S: TInflateBlocksState; Var Z: TZState; R: Integer): Integer;

// R contains the initial return code
                         
Var
  Temp: Cardinal;
  B: Cardinal;    // bit buffer
  K: Cardinal;    // bits in bit buffer
  P: PByte;       // input data pointer
  N: Cardinal;    // bytes available there
  Q: PByte;       // output Window write pointer
  M: Cardinal;    // bytes to end of window or read pointer 
  // fixed code blocks 
  LiteralBits,
  DistanceBits: Cardinal;
  TL,
  TD: PInflateHuft;
  H: PInflateHuft;
  I, J, C: Cardinal;
  CodeState: PInflateCodesState;

  //--------------- local functions -------------------------------------------

  Function UpdatePointers: Integer;

  Begin
    S.bitb := B;
    S.bitk := K;
    Z.AvailableInput := N;
    Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
    Z.NextInput := P;
    S.Write := Q;
    Result := InflateFlush(S, Z, R);
  End;

  //--------------- end local functions ---------------------------------------

Begin
  // copy input/output information to locals 
  P := Z.NextInput;
  N := Z.AvailableInput;
  B := S.bitb;
  K := S.bitk;
  Q := S.Write;
  If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                    Else M := Cardinal(S.zend) - Cardinal(Q);

  // decompress an inflated block 
  // process input based on current state
  While True Do
  Begin
    Case S.mode Of
      ibmZType:
        Begin
          While K < 3 Do
          Begin
            If N <> 0 Then R := Z_OK
                      Else
            Begin
              Result := UpdatePointers;
              Exit;
            End;
            Dec(N);
            B := B Or (Cardinal(P^) Shl K);
            Inc(P);
            Inc(K, 8);
          End;

          Temp := B And 7;
          S.last := Boolean(Temp And 1);
          Case Temp Shr 1 Of
            0: // stored
              Begin
                B := B Shr 3;
                Dec(K, 3);
                // go to byte boundary
                Temp := K And 7;
                B := B Shr Temp;
                Dec(K, Temp);
                // get length of stored block
                S.mode := ibmLens;
              End;
            1: // fixed
              Begin
                InflateTreesFixed(LiteralBits, DistanceBits, TL, TD, Z);
                S.sub.decode.codes := InflateCodesNew(LiteralBits, DistanceBits, TL, TD, Z);
                If S.sub.decode.codes = Nil Then
                Begin
                  R := Z_MEM_ERROR;
                  Result := UpdatePointers;
                  Exit;
                End;
                B := B Shr 3;
                Dec(K, 3);
                S.mode := ibmCodes;
              End;
            2: // dynamic
              Begin
                B := B Shr 3;
                Dec(K, 3);
                S.mode := ibmTable;
              End;
            3: // illegal
              Begin
                B := B Shr 3;
                Dec(K, 3);
                S.mode := ibmBlockBad;
                Z.Msg := SInvalidBlockType;
                R := Z_DATA_ERROR;
                Result := UpdatePointers;
                Exit;
              End;
          End;
        End;
      ibmLens:
        Begin
          While K < 32 Do
          Begin
            If N <> 0 Then R := Z_OK
                      Else
            Begin
              Result := UpdatePointers;
              Exit;
            End;
            Dec(N);
            B := B Or (Cardinal(P^) Shl K);
            Inc(P);
            Inc(K, 8);
          End;

          If (((Not B) Shr 16) And $FFFF) <> (B And $FFFF) Then
          Begin
            S.mode := ibmBlockBad;
            Z.Msg := SInvalidStoredBlockLengths;
            R := Z_DATA_ERROR;
            Result := UpdatePointers;
            Exit;
          End;
          S.sub.left := B And $FFFF;
          K := 0;
          B := 0;
          If S.sub.left <> 0 Then S.mode := ibmStored
                             Else
            If S.last Then S.mode := ibmDry
                      Else S.mode := ibmZType;
        End;
      ibmStored:
        Begin
          If N = 0 Then
          Begin
            Result := UpdatePointers;
            Exit;
          End;

          If M = 0 Then
          Begin
            If (Q = S.zend) And (S.Read <> S.Window) Then
            Begin
              Q := S.Window;
              If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                Else M := Cardinal(S.zend) - Cardinal(Q);
            End;

            If M = 0 Then
            Begin
              S.Write := Q;
              R := InflateFlush(S, Z, R);
              Q := S.Write;
              If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                Else M := Cardinal(S.zend) - Cardinal(Q);
              If (Q = S.zend) And (S.Read <> S.Window) Then
              Begin
                Q := S.Window;
                If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                                  Else M := Cardinal(S.zend) - Cardinal(Q);
              End;

              If M = 0 Then
              Begin
                Result := UpdatePointers;
                Exit;
              End;
            End;
          End;
          R := Z_OK;

          Temp := S.sub.left;
          If Temp > N Then Temp := N;
          If Temp > M Then Temp := M;
          Move(P^, Q^, Temp);
          Inc(P, Temp);
          Dec(N, Temp);
          Inc(Q, Temp);
          Dec(M, Temp);
          Dec(S.sub.left, Temp);
          If S.sub.left = 0 Then
          Begin
            If S.last Then S.mode := ibmDry
                      Else S.mode := ibmZType;
          End;
        End;
      ibmTable:
        Begin
          While K < 14 Do
          Begin
            If N <> 0 Then R := Z_OK
                      Else
            Begin
              Result := UpdatePointers;
              Exit;
            End;
            Dec(N);
            B := B Or (Cardinal(P^) Shl K);
            Inc(P);
            Inc(K, 8);
          End;

          Temp := B And $3FFF;
          S.sub.trees.table := Temp;
          If ((Temp And $1F) > 29) Or (((Temp Shr 5) And $1F) > 29) Then
          Begin
            S.mode := ibmBlockBad;
            Z.Msg := STooManyLDSymbols;
            R := Z_DATA_ERROR;
            Result := UpdatePointers;
            Exit;
          End;
          Temp := 258 + (Temp And $1F) + ((Temp Shr 5) And $1F);
          Try
            S.sub.trees.blens := AllocMem(Temp * SizeOf(Cardinal));
          Except
            R := Z_MEM_ERROR;
            UpdatePointers;
            Raise;
          End;
          B := B Shr 14;
          Dec(K, 14);

          S.sub.trees.Index := 0;
          S.mode := ibmBitTree;
        End;
      ibmBitTree:
        Begin
          While (S.sub.trees.Index < 4 + (S.sub.trees.table Shr 10)) Do
          Begin
            While K < 3 Do
            Begin
              If N <> 0 Then R := Z_OK
                        Else
              Begin
                Result := UpdatePointers;
                Exit;
              End;
              Dec(N);
              B := B Or (Cardinal(P^) Shl K);
              Inc(P);
              Inc(K, 8);
            End;

            S.sub.trees.blens[BitOrder[S.sub.trees.Index]] := B And 7;
            Inc(S.sub.trees.Index);
            B := B Shr 3;
            Dec(K, 3);
          End;

          While S.sub.trees.Index < 19 Do
          Begin
            S.sub.trees.blens[BitOrder[S.sub.trees.Index]] := 0;
            Inc(S.sub.trees.Index);
          End;
          S.sub.trees.BB := 7;
          Temp := InflateTreesBits(S.sub.trees.blens^, S.sub.trees.BB, S.sub.trees.TB, S.hufts^, Z);
          If Temp <> Z_OK Then
          Begin
            FreeMem(S.sub.trees.blens);
            R := Temp;
            If R = Z_DATA_ERROR Then S.mode := ibmBlockBad;
            Result := UpdatePointers;
            Exit;
          End;
          S.sub.trees.Index := 0;
          S.mode := ibmDistTree;
        End;
      ibmDistTree:
        Begin
          While True Do
          Begin
            Temp := S.sub.trees.table;
            If Not (S.sub.trees.Index < 258 + (Temp And $1F) + ((Temp Shr 5) And $1F)) Then Break;
            Temp := S.sub.trees.BB;
            While K < Temp Do
            Begin
              If N <> 0 Then R := Z_OK
                        Else
              Begin
                Result := UpdatePointers;
                Exit;
              End;
              Dec(N);
              B := B Or (Cardinal(P^) Shl K);
              Inc(P);
              Inc(K, 8);
            End;

            H := S.sub.trees.TB;
            Inc(H, B And InflateMask[Temp]);
            Temp := H^.Bits;
            C := H^.Base;

            If C < 16 Then
            Begin
              B := B Shr Temp;
              Dec(K, Temp);
              S.sub.trees.blens^[S.sub.trees.Index] := C;
              Inc(S.sub.trees.Index);
            End
            Else
            Begin
              // C = 16..18
              If C = 18 Then
              Begin
                I := 7;
                J := 11;
              End
              Else
              Begin
                I := C - 14;
                J := 3;
              End;

              While K < Temp + I Do
              Begin
                If N <> 0 Then R := Z_OK
                          Else
                Begin
                  Result := UpdatePointers;
                  Exit;
                End;
                Dec(N);
                B := B Or (Cardinal(P^) Shl K);
                Inc(P);
                Inc(K, 8);
              End;

              B := B Shr Temp;
              Dec(K, Temp);

              Inc(J, Cardinal(B) And InflateMask[I]);
              B := B Shr I;
              Dec(K, I);

              I := S.sub.trees.Index;
              Temp := S.sub.trees.table;
              If (I + J > 258 + (Temp And $1F) + ((Temp Shr 5) And $1F)) Or ((C = 16) And (I < 1)) Then
              Begin
                FreeMem(S.sub.trees.blens);
                S.mode := ibmBlockBad;
                Z.Msg := SInvalidBitLengthRepeat;
                R := Z_DATA_ERROR;
                Result := UpdatePointers;
                Exit;
              End;

              If C = 16 Then C := S.sub.trees.blens[I - 1]
                        Else C := 0;
              Repeat
                S.sub.trees.blens[I] := C;
                Inc(I);
                Dec(J);
              Until J = 0;
              S.sub.trees.Index := I;
            End;
          End; // while

          S.sub.trees.TB := Nil;
          Begin
            LiteralBits := 9;
            DistanceBits := 6;
            Temp := S.sub.trees.table;
            Temp := InflateTreesDynamic(257 + (Temp And $1F), 1 + ((Temp Shr 5) And $1F),
                                        S.sub.trees.blens^, LiteralBits, DistanceBits, TL, TD, S.hufts^, Z);
            FreeMem(S.sub.trees.blens);
            If Temp <> Z_OK Then
            Begin
              If Integer(Temp) = Z_DATA_ERROR Then S.mode := ibmBlockBad;
              R := Temp;
              Result := UpdatePointers;
              Exit;
            End;
            CodeState := InflateCodesNew(LiteralBits, DistanceBits, TL, TD, Z);
            If CodeState = Nil Then
            Begin
              R := Z_MEM_ERROR;
              Result := UpdatePointers;
              Exit;
            End;
            S.sub.decode.codes := CodeState;
          End;
          S.mode := ibmCodes;
        End;
      ibmCodes:
        Begin
          // update pointers
          S.bitb := B;
          S.bitk := K;
          Z.AvailableInput := N;
          Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
          Z.NextInput := P;
          S.Write := Q;
          R := InflateCodes(S, Z, R);

          // very strange, I have no clue why the local function does not work here...
          // R := UpdatePointers;
          If R <> Z_STREAM_END Then
          Begin
            Result := InflateFlush(S, Z, R);
            Exit;
          End;
          R := Z_OK;
          Freemem(S.sub.decode.codes);
          // load local pointers
          P := Z.NextInput;
          N := Z.AvailableInput;
          B := S.bitb;
          K := S.bitk;
          Q := S.Write;
          If Cardinal(Q) < Cardinal(S.Read) Then M := Cardinal(S.Read) - Cardinal(Q) - 1
                                            Else M := Cardinal(S.zend) - Cardinal(Q);
          If Not S.last Then
          Begin
            S.mode := ibmZType;
            Continue;  
          End;
          S.mode := ibmDry;
        End;
      ibmDry:
        Begin
          S.Write := Q;
          R := InflateFlush(S, Z, R);
          Q := S.Write;

          If S.Read <> S.Write Then
          Begin
            Result := UpdatePointers;
            Exit;
          End;
          S.mode := ibmBlockDone;
        End;
      ibmBlockDone:
        Begin
          R := Z_STREAM_END;
          Result := UpdatePointers;
          Exit;
        End;
      ibmBlockBad:
        Begin
          R := Z_DATA_ERROR;
          Result := UpdatePointers;
          Exit;
        End;
    Else
      R := Z_STREAM_ERROR;
      Result := UpdatePointers;
      Exit;
    End; // case S.mode of
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateBlocksFree(S: PInflateBlocksState; Var Z: TZState): Integer;

Begin
  InflateBlockReset(S^, Z, Nil);
  FreeMem(S.Window);
  FreeMem(S.hufts);
  FreeMem(S);
  Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

Function IsInflateBlocksSynchPoint(Var S: TInflateBlocksState): Boolean;

// returns True if Inflate is currently at the end of a block generated by Z_SYNC_FLUSH or Z_FULL_FLUSH

Begin
  Result := S.mode = ibmLens;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateReset(Var Z: TZState): Integer;

// This function is equivalent to InflateEnd followed by InflateInit, but does not free and reallocate all the internal
// decompression state. The stream will keep attributes that may have been set by InflateInit2.
//
// InflateReset returns Z_OK if success, or Z_STREAM_ERROR if the Source
// stream state was inconsistent (such State being nil).

Begin
  If Z.State = Nil Then Result :=  Z_STREAM_ERROR
                   Else
  Begin
    Z.TotalOutput := 0;
    Z.TotalInput := 0;
    Z.Msg := '';
    If Z.State.nowrap Then Z.State.mode := imBlocks
                      Else Z.State.mode := imMethod;
    InflateBlockReset(Z.State.blocks^, Z, Nil);
    Result := Z_OK;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateEnd(Var Z: TZState): Integer;

// All dynamically allocated data structures for this stream are freed. This function discards any unprocessed input and
// does not flush any pending output.
//
// InflateEnd returns Z_OK on success, Z_STREAM_ERROR if the stream state was inconsistent. 

Begin
  If Z.State = Nil Then Result :=  Z_STREAM_ERROR
                   Else
  Begin
    If Assigned(Z.State.blocks) Then InflateBlocksFree(Z.State.blocks, Z);
    FreeMem(Z.State);
    Z.State := Nil;
    Result := Z_OK;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateInit2_(Var Z: TZState; W: Integer; Const Version: AnsiString; StreamSize: Integer): Integer;

Begin
  If (Version = '') Or
     (Version[1] <> ZLIB_VERSION[1]) Or
     (StreamSize <> SizeOf(TZState)) Then Result := Z_VERSION_ERROR
                                     Else
  Begin
    // initialize state
    Z.Msg := '';
    Z.State := AllocMem(SizeOf(TInternalState));

    // handle undocumented nowrap option (no zlib header or check)
    If W < 0 Then
    Begin
      W := - W;
      Z.State.nowrap := True;
    End;

    // set window size
    If (W < 8) Or (W > 15) Then
    Begin
      InflateEnd(Z);
      Result := Z_STREAM_ERROR;
      Exit;
    End;
    Z.State.wbits := W;

    // create InflateBlocks state
    If Z.State.nowrap Then Z.State.blocks := InflateBlocksNew(Z, Nil, 1 Shl W)
                      Else Z.State.blocks := InflateBlocksNew(Z, Adler32, 1 Shl W);
    If Z.State.blocks = Nil Then
    Begin
      InflateEnd(Z);
      Result := Z_MEM_ERROR;
      Exit;
    End;
    // reset state
    InflateReset(Z);
    Result := Z_OK;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateInit2(Var Z: TZState; AWindowBits: Integer): Integer;

// This is another Version of InflateInit with an extra parameter. The fields NextInput and AvailableInput must be
// initialized before by the caller.
//
// The WindowBits parameter is the base two logarithm of the maximum window size (the Size of the history buffer). It
// should be in the range 8..15 for this version of the library. The default value is 15 if InflateInit is used instead.
// If a compressed stream with a larger window size is given as input, Inflate will return with the error code
// Z_DATA_ERROR instead of trying to allocate a larger window.
//
// InflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_STREAM_ERROR if a parameter is
// invalid (such as a negative MemLevel). Msg is reset if there is no error message.  InflateInit2 does not perform any
// decompression apart from reading the zlib Header if present, this will be done by Inflate. (So NextInput and
// AvailableInput may be modified, but NextOutput and AvailableOutput are unchanged.)

Begin
  Result := InflateInit2_(Z, AWindowBits, ZLIB_VERSION, SizeOf(TZState));
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateInit(Var Z: TZState): Integer;

// Initializes the internal stream state for decompression. 
//
// InflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_VERSION_ERROR if the zlib library
// version is incompatible with the version assumed by the caller. Msg is reset if there is no
// error message. InflateInit does not perform any decompression: this will be done by Inflate.

Begin
  Result := InflateInit2_(Z, DEF_WBITS, ZLIB_VERSION, SizeOf(TZState));
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateInit_(Var Z: TZState; Const Version: String; StreamSize: Integer): Integer;

Begin
  Result := InflateInit2_(Z, DEF_WBITS, Version, StreamSize);
End;

//----------------------------------------------------------------------------------------------------------------------

Function Inflate(Var Z: TZState; F: Integer): Integer;

// Inflate decompresses as much data as possible and stops when the input buffer becomes empty or the output buffer
// becomes full. It may introduce some output latency (reading input without producing any output) except when forced to
// flush.
//
// The detailed semantics are as follows. Inflate performs one or both of the following actions:
// - Decompress more input starting at NextInput and update NextInput and AvailableInput accordingly. if not all input
//   can be processed (because there is not enough room in the output buffer), NextInput is updated and processing will
//   resume at this point for the next call of Inflate.
//
// - Provide more output starting at NextOutput and update NextOutput and AvailableOutput accordingly. Inflate provides
//   as much output as possible, until there is no more input data or no more space in the output buffer (see below
//   about the Flush parameter).
//
// Before the call of Inflate the application should ensure that at least one of the actions is possible, by providing
// more input and/or consuming more output, and updating the Next* and Avail* values accordingly. The application can
// consume the uncompressed output when it wants, for example when the output buffer is full (AvailableOutput = 0), or
// after each call of Inflate. If Inflate returns Z_OK and with zero AvailableOutput, it must be called again after
// making room in the output buffer because there might be more output pending.
//
// If the parameter Flush is set to Z_SYNC_FLUSH, Inflate flushes as much output as possible to the output buffer. The
// flushing behavior of Inflate is not specified for values of the Flush parameter other than Z_SYNC_FLUSH and Z_FINISH,
// but the current implementation actually flushes as much output as possible anyway.
//
// Inflate should normally be called until it returns Z_STREAM_END or an error. However if all decompression is to be
// performed in a single step (a single call of Inflate), the parameter Flush should be set to Z_FINISH. In this case
// all pending input is processed and all pending output is flushed; AvailableOutput must be large enough to hold all
// the uncompressed data. (The size of the uncompressed data may have been saved by the compressor for this purpose.)
// The next operation on this stream must be InflateEnd to deallocate the decompression State. The use of Z_FINISH is
// never required, but can be used to inform Inflate that a faster routine may be used for the single Inflate call.
//
// if a preset dictionary is needed at this point (see InflateSetDictionary below), Inflate sets ZState.Adler to the
// Adler32 checksum of the dictionary chosen by the compressor and returns Z_NEED_DICT. Otherwise it sets ZState.Adler
// to the Adler32 checksum of all output produced so far (that is, TotalOutput bytes) and returns Z_OK, Z_STREAM_END or
// an error code as described below. At the end of the stream, Inflate checks that its computed Adler32 checksum is
// equal to that saved by the compressor and returns Z_STREAM_END only if the checksum is correct.
//
// Inflate returns Z_OK if some progress has been made (more input processed or more output produced), Z_STREAM_END if
// the end of the compressed data has been reached and all uncompressed output has been produced, Z_NEED_DICT if a
// preset dictionary is needed at this point, Z_DATA_ERROR if the input data was corrupted (input stream not conforming
// to the zlib format or incorrect Adler32 checksum), Z_STREAM_ERROR if the stream structure was inconsistent (for
// example if NextInput or NextOutput was nil), Z_MEM_ERROR if there was not enough memory, Z_BUF_ERROR if no progress
// is possible or if there was not enough room in the output buffer when Z_FINISH is used. In the Z_DATA_ERROR
// case, the application may then call InflateSync to look for a good compression block.

Var
  R: Integer;
  B: Cardinal;

Begin
  If (Z.State = Nil) Or (Z.NextInput = Nil) Then Result := Z_STREAM_ERROR
                                            Else
  Begin
    If F = Z_FINISH Then F := Z_BUF_ERROR
                    Else F := Z_OK;
    R := Z_BUF_ERROR;
    While True Do
    Begin
      Case Z.State.mode Of
        imBlocks:
          Begin
            R := InflateBlocks(Z.State.blocks^, Z, R);
            If R = Z_DATA_ERROR Then
            Begin
              Z.State.mode := imBad;
              // can try InflateSync
              Z.State.sub.marker := 0;
              Continue;
            End;

            If R = Z_OK Then R := F;
            If R <> Z_STREAM_END Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;
            InflateBlockReset(Z.State.blocks^, Z, @Z.State.sub.Check.was);
            If Z.State.nowrap Then
            Begin
              Z.State.mode := imDone;
              Continue;            
            End;
            Z.State.mode := imCheck4;  
          End;
        imCheck4:
          Begin
            If (Z.AvailableInput = 0) Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;

            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Z.State.sub.Check.need := Cardinal(Z.NextInput^) Shl 24;
            Inc(Z.NextInput);

            Z.State.mode := imCheck3;    
          End;
        imCheck3:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;
            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Inc(Z.State.sub.Check.need, Cardinal(Z.NextInput^) Shl 16);
            Inc(Z.NextInput);

            Z.State.mode := imCheck2;   
          End;
        imCheck2:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;

            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Inc(Z.State.sub.Check.need, Cardinal(Z.NextInput^) Shl 8);
            Inc(Z.NextInput);

            Z.State.mode := imCheck1;    
          End;
        imCheck1:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;
            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Inc(Z.State.sub.Check.need, Cardinal(Z.NextInput^));
            Inc(Z.NextInput);

            If Z.State.sub.Check.was <> Z.State.sub.Check.need Then
            Begin
              Z.State.mode := imBad;
              Z.Msg := SIncorrectDataCheck;
              // can't try InflateSync
              Z.State.sub.marker := 5;
              Continue;
            End;
            Z.State.mode := imDone;  
          End;
        imDone:
          Begin
            Result := Z_STREAM_END;
            Exit;
          End;
        imMethod:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F; 

            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Z.State.sub.imMethod := Z.NextInput^;
            Inc(Z.NextInput);

            If (Z.State.sub.imMethod And $0F) <> Z_DEFLATED Then
            Begin
              Z.State.mode := imBad;
              Z.Msg := SUnknownCompression;
              // can't try InflateSync
              Z.State.sub.marker := 5;
              Continue;   
            End;

            If (Z.State.sub.imMethod Shr 4) + 8 > Z.State.wbits Then
            Begin
              Z.State.mode := imBad;
              Z.Msg := SInvalidWindowSize;
              // can't try InflateSync
              Z.State.sub.marker := 5;
              Continue;  
            End;
            Z.State.mode := imFlag;
          End;
        imFlag:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F; 
            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            B := Z.NextInput^;
            Inc(Z.NextInput);

            If (((Z.State.sub.imMethod Shl 8) + B) Mod 31) <> 0 Then
            Begin
              Z.State.mode := imBad;
              Z.Msg := SIncorrectHeaderCheck;
              // can't try InflateSync
              Z.State.sub.marker := 5;
              Continue;       
            End;

            If (B And PRESET_DICT) = 0 Then
            Begin
              Z.State.mode := imBlocks;
              Continue;
            End;
            Z.State.mode := imDict4;
          End;
        imDict4:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;

            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Z.State.sub.Check.need :=  Cardinal(Z.NextInput^) Shl 24;
            Inc(Z.NextInput);

            Z.State.mode := imDict3;    
          End;
        imDict3:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;
            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Inc(Z.State.sub.Check.need, Cardinal(Z.NextInput^) Shl 16);
            Inc(Z.NextInput);

            Z.State.mode := imDict2;         
          End;
        imDict2:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            R := F;

            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Inc(Z.State.sub.Check.need, Cardinal(Z.NextInput^) Shl 8);
            Inc(Z.NextInput);

            Z.State.mode := imDict1;         
          End;
        imDict1:
          Begin
            If Z.AvailableInput = 0 Then
            Begin
              Result := R;
              Exit;
            End;
            Dec(Z.AvailableInput);
            Inc(Z.TotalInput);
            Inc(Z.State.sub.Check.need, Cardinal(Z.NextInput^) );
            Inc(Z.NextInput);

            Z.Adler := Z.State.sub.Check.need;
            Z.State.mode := imDict0;
            Inflate := Z_NEED_DICT;
            Exit;
          End;
        imDict0:
          Begin
            Z.State.mode := imBad;
            Z.Msg := SNeedDictionary;
            // can try InflateSync
            Z.State.sub.marker := 0;
            Inflate := Z_STREAM_ERROR;
            Exit;
          End;
        imBad:
          Begin
            Result := Z_DATA_ERROR;
            Exit;
          End;
        Else
          Begin
            Result := Z_STREAM_ERROR;
            Exit;
          End;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateSetDictionary(Var Z: TZState; Dictionary: PByte; DictLength: Cardinal): Integer;

// Initializes the decompression dictionary from the given uncompressed byte sequence. This function must be called
// immediately after a call of Inflate if this call returned Z_NEED_DICT. The dictionary chosen by the compressor
// can be determined from the Adler32 Value returned by this call of Inflate. The compressor and decompressor must use
// exactly the same dictionary (see DeflateSetDictionary).
//
// InflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a parameter is invalid (such as nil dictionary) or
// the stream state is inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the expected one (incorrect
// Adler32 Value). InflateSetDictionary does not perform any decompression: this will be done by subsequent calls of Inflate.

Var
  Length: Cardinal;

Begin
  Length := DictLength;

  If (Z.State = Nil) Or (Z.State.mode <> imDict0) Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  If Adler32(1, Dictionary, DictLength) <> Z.Adler Then
  Begin
    Result := Z_DATA_ERROR;
    Exit;
  End;

  Z.Adler := 1;

  If Length >= (1 Shl Z.State.wbits) Then
  Begin
    Length := (1 Shl Z.State.wbits) - 1;
    Inc( Dictionary, DictLength - Length);
  End;

  With Z.State.blocks^ Do
  Begin
    Move(Dictionary^, Window^, Length);
    Write := Window;
    Inc(Write, Length);
    Read := Write;
  End;
  Z.State.mode := imBlocks;
  Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

Function InflateSync(Var Z: TZState): Integer;

// Skips invalid compressed data until a full flush point (see above the description of Deflate with Z_FULL_FLUSH) can
// be found, or until all available input is skipped. No output is provided.
//
// InflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR if no more input was provided,
// Z_DATA_ERROR if no flush point has been found, or Z_STREAM_ERROR if the stream structure was inconsistent. In the
// success case, the application may save the current current value of TotalInput which indicates where valid compressed
// data was found. In the error case, the application may repeatedly call InflateSync, providing more input each time,
// until success or end of the input data.

Const
  Mark: Packed Array[0..3] Of Byte = (0, 0, $FF, $FF);

Var
  N: Cardinal;    // number of bytes to look at
  P: PByte;       // pointer to bytes
  M: Cardinal;    // number of marker bytes found in a row
  R, W: Cardinal; // temporaries to save TotalInput and TotalOutput

Begin
  If Z.State = Nil Then
  Begin
    Result := Z_STREAM_ERROR;
    Exit;
  End;

  If Z.State.mode <> imBad Then
  Begin
    Z.State.mode := imBad;
    Z.State.sub.marker := 0;
  End;

  N := Z.AvailableInput;
  If N = 0 Then
  Begin
    Result := Z_BUF_ERROR;
    Exit;
  End;

  P := Z.NextInput;
  M := Z.State.sub.marker;

  // search
  While (N <> 0) And (M < 4) Do
  Begin
    If P^ = Mark[M] Then Inc(M)
                    Else
      If P^ <> 0 Then M := 0
                 Else M := 4 - M;
    Inc(P);
    Dec(N);
  End;

  // restore
  Inc(Z.TotalInput, Cardinal(P) - Cardinal(Z.NextInput));
  Z.NextInput := P;
  Z.AvailableInput := N;
  Z.State.sub.marker := M;

  // return no joy or set up to restart on a new block
  If M <> 4 Then
  Begin
    Result := Z_DATA_ERROR;
    Exit;
  End;

  R := Z.TotalInput;
  W := Z.TotalOutput;
  InflateReset(Z);
  Z.TotalInput := R;
  Z.TotalOutput := W;
  Z.State.mode := imBlocks;
  Result := Z_OK;
End;

//----------------------------------------------------------------------------------------------------------------------

Function IsInflateSyncPoint(Var Z: TZState): Integer;

// Returns 1 if Inflate is currently at the end of a block generated by Z_SYNC_FLUSH or Z_FULL_FLUSH.
// This function is used by one PPP implementation to provide an additional safety Check. PPP uses Z_SYNC_FLUSH but
// removes the length bytes of the resulting empty stored block. When decompressing, PPP checks that at the end of input
// packet, Inflate is waiting for these length bytes.

Begin
  If (Z.State = Nil) Or (Z.State.blocks = Nil) Then Result := Z_STREAM_ERROR
                                               Else Result := Ord(IsInflateBlocksSynchPoint(Z.State.blocks^));
End;

//----------------------------------------------------------------------------------------------------------------------

End.

