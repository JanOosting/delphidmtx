unit dmtx;
interface
uses windows;
(*
translation of dmtx.h
/**
 * libdmtx - Data Matrix Encoding/Decoding Library
 * Copyright 2008, 2009 Mike Laughton. All rights reserved.
 *
 * See LICENSE file in the main project directory for full
 * terms of use and distribution.
 *
 * Contact: Mike Laughton <mike@dragonflylogic.com>
 *
 * \file dmtx.h
 * \brief Main libdmtx header
 */
*)
// translation by Jan Oosting:  j.oosting@lumc.nl

// Version of dmtx.h that this file conforms to
// source tarball of 0.7.4

// only the high level functions of the dll have been translated
//  these are sufficient to create and scan datamatrix barcodes

// untranslated sections are kept in comments

// enums get an extra member with value $10000 to make sure sizeof is large enough in structures/records
//   MingW = 4 bytes,  Delphi= 1 byte if high() < 256


//#ifndef __DMTX_H__
//#define __DMTX_H__

//#ifdef __cplusplus
//extern "C" {
//#endif

//* Time headers required for DmtxTime struct below */
//#include <time.h>
//#ifdef HAVE_SYS_TIME_H
//#include <sys/time.h>
//#endif
const
 M_PI   =  3.14159265358979323846;
 M_PI_2 = 1.57079632679489661923;

 DmtxVersion   =  '0.7.4';

 DmtxUndefined =  -1;

//#define DmtxPassFail        unsigned int
 DmtxPass      =   1;
 DmtxFail      =   0;

//#define DmtxBoolean         unsigned int
 DmtxTrue      =   1;
 DmtxFalse     =   0;

 DmtxFormatMatrix  =   0;
 DmtxFormatMosaic  =   1;

 DmtxSymbolSquareCount =  24;
 DmtxSymbolRectCount   =   6;

 DmtxModuleOff         =      $00;
 DmtxModuleOnRed       =      $01;
 DmtxModuleOnGreen     =      $02;
 DmtxModuleOnBlue      =      $04;
 DmtxModuleOnRGB       =      $07;  //* OnRed | OnGreen | OnBlue */
 DmtxModuleOn          =      $07;
 DmtxModuleUnsure      =      $08;
 DmtxModuleAssigned    =      $10;
 DmtxModuleVisited     =      $20;
 DmtxModuleData        =      $40;

//#define DMTX_CHECK_BOUNDS(l,i) (assert((i) >= 0 && (i) < (l)->length && (l)->length <= (l)->capacity))
type
DmtxStatus =(
   DmtxStatusEncoding, //* Encoding is currently underway */
   DmtxStatusComplete, //* Encoding is done and everything went well */
   DmtxStatusInvalid,  //* Something bad happened that sometimes happens */
   DmtxStatusFatal,     //* Something happened that should never happen */
   
   DmtxStatusLast=$10000 // make sure sizeof is large enough in structures/records
);

DmtxScheme =(
   DmtxSchemeAutoFast        = -2,
   DmtxSchemeAutoBest        = -1,
   DmtxSchemeAscii           =  0,
   DmtxSchemeC40,
   DmtxSchemeText,
   DmtxSchemeX12,
   DmtxSchemeEdifact,
   DmtxSchemeBase256,

   DmtxSchemeLast=$10000 // make sure sizeof is large enough in structures/records
);

DmtxSymbolSize =(
   DmtxSymbolRectAuto        = -3,
   DmtxSymbolSquareAuto      = -2,
   DmtxSymbolShapeAuto       = -1,
   DmtxSymbol10x10           =  0,
   DmtxSymbol12x12,
   DmtxSymbol14x14,
   DmtxSymbol16x16,
   DmtxSymbol18x18,
   DmtxSymbol20x20,
   DmtxSymbol22x22,
   DmtxSymbol24x24,
   DmtxSymbol26x26,
   DmtxSymbol32x32,
   DmtxSymbol36x36,
   DmtxSymbol40x40,
   DmtxSymbol44x44,
   DmtxSymbol48x48,
   DmtxSymbol52x52,
   DmtxSymbol64x64,
   DmtxSymbol72x72,
   DmtxSymbol80x80,
   DmtxSymbol88x88,
   DmtxSymbol96x96,
   DmtxSymbol104x104,
   DmtxSymbol120x120,
   DmtxSymbol132x132,
   DmtxSymbol144x144,
   DmtxSymbol8x18,
   DmtxSymbol8x32,
   DmtxSymbol12x26,
   DmtxSymbol12x36,
   DmtxSymbol16x36,
   DmtxSymbol16x48,
   
   DmtxSymbolLast=$10000 // make sure sizeof is large enough in structures/records
);

(*typedef enum {
   DmtxDirNone               = 0x00,
   DmtxDirUp                 = 0x01 << 0,
   DmtxDirLeft               = 0x01 << 1,
   DmtxDirDown               = 0x01 << 2,
   DmtxDirRight              = 0x01 << 3,
   DmtxDirHorizontal         = DmtxDirLeft  | DmtxDirRight,
   DmtxDirVertical           = DmtxDirUp    | DmtxDirDown,
   DmtxDirRightUp            = DmtxDirRight | DmtxDirUp,
   DmtxDirLeftDown           = DmtxDirLeft  | DmtxDirDown
} DmtxDirection;*)
DmtxDirection = (
  DmtxDirNone=0,
  DmtxDirUp=1,
  DmtxDirLeft=2,
  DmtxDirDown=4,
  DmtxDirVertical=5,
  DmtxDirLeftDown=6,
  DmtxDirRight=8,
  DmtxDirRightUp=9,
  DmtxDirHorizontal=10,

  DmtxDirLast=$10000   // make sure sizeof is large enough in structures/records
);
DmtxSymAttribute=(
   DmtxSymAttribSymbolRows,
   DmtxSymAttribSymbolCols,
   DmtxSymAttribDataRegionRows,
   DmtxSymAttribDataRegionCols,
   DmtxSymAttribHorizDataRegions,
   DmtxSymAttribVertDataRegions,
   DmtxSymAttribMappingMatrixRows,
   DmtxSymAttribMappingMatrixCols,
   DmtxSymAttribInterleavedBlocks,
   DmtxSymAttribBlockErrorWords,
   DmtxSymAttribBlockMaxCorrectable,
   DmtxSymAttribSymbolDataWords,
   DmtxSymAttribSymbolErrorWords,
   DmtxSymAttribSymbolMaxCorrectable,
   
   DmtxSymAttribLast=$10000 // make sure sizeof is large enough in structures/records
);

DmtxCornerLoc=(
   DmtxCorner00 =1, //             = 0x01 << 0,
   DmtxCorner10 =2, //             = 0x01 << 1,
   DmtxCorner11 =4, //             = 0x01 << 2,
   DmtxCorner01 =8, //             = 0x01 << 3,
   
   DmtxCornerLast=$10000 // make sure sizeof is large enough in structures/records
);

DmtxProperty=(
   //* Encoding properties */
   DmtxPropScheme            = 100,
   DmtxPropSizeRequest,
   DmtxPropMarginSize,
   DmtxPropModuleSize,
   //* Decoding properties */
   DmtxPropEdgeMin           = 200,
   DmtxPropEdgeMax,
   DmtxPropScanGap,
   DmtxPropSquareDevn,
   DmtxPropSymbolSize,
   DmtxPropEdgeThresh,
   //* Image properties */
   DmtxPropWidth             = 300,
   DmtxPropHeight,
   DmtxPropPixelPacking,
   DmtxPropBitsPerPixel,
   DmtxPropBytesPerPixel,
   DmtxPropRowPadBytes,
   DmtxPropRowSizeBytes,
   DmtxPropImageFlip,
   DmtxPropChannelCount,
   //* Image modifiers */
   DmtxPropXmin              = 400,
   DmtxPropXmax,
   DmtxPropYmin,
   DmtxPropYmax,
   DmtxPropScale,

   DmtxPropLast=$10000 // make sure sizeof is large enough in structures/records
);

DmtxPackOrder=(
   //* Custom format */
   DmtxPackCustom            = 100,
   //* 1 bpp */
   DmtxPack1bppK             = 200,
   //* 8 bpp grayscale */
   DmtxPack8bppK             = 300,
   //* 16 bpp formats */
   DmtxPack16bppRGB          = 400,
   DmtxPack16bppRGBX,
   DmtxPack16bppXRGB,
   DmtxPack16bppBGR,
   DmtxPack16bppBGRX,
   DmtxPack16bppXBGR,
   DmtxPack16bppYCbCr,
   //* 24 bpp formats */
   DmtxPack24bppRGB          = 500,
   DmtxPack24bppBGR,
   DmtxPack24bppYCbCr,
   //* 32 bpp formats */
   DmtxPack32bppRGBX         = 600,
   DmtxPack32bppXRGB,
   DmtxPack32bppBGRX,
   DmtxPack32bppXBGR,
   DmtxPack32bppCMYK,

   DmtxPackLast=$10000 // make sure sizeof is large enough in structures/records
);

DmtxFlip=(
  DmtxFlipNone = 0, //              = 0x00,
  DmtxFlipX    = 1, //              = 0x01 << 0,
  DmtxFlipY    = 2, //              = 0x01 << 1,

  DmtxFlipLast=$10000 // make sure sizeof is large enough in structures/records
);

DmtxMatrix3 = array[0..2,0..2] of double;

DmtxPixelLoc = record
   X :integer;
   Y :integer;
end;

pDmtxVector2=^DmtxVector2;
DmtxVector2 = record
  X :double;
  Y :double;
end ;

PDmtxRay2=^DmtxRay2;
DmtxRay2 = record
  tMin, tMax :double;
  p          :DmtxVector2 ;
  v          :DmtxVector2;
end;

pDmtxByte=^DmtxByte;
DmtxByte = Byte;

// * Use signed int for length fields instead of size_t to play nicely with RS
// * arithmetic
pDmtxByteList=^DmtxByteList;
DmtxByteList=record
  length, capacity : integer; 
  b : pDmtxByte;
end;

DmtxEncodeStream=record
   currentScheme:integer;         //* Current encodation scheme */
   inputNext:integer;             //* Index of next unprocessed input word in queue */
   outputChainValueCount:integer; //* Count of output values pushed within current scheme chain */
   outputChainWordCount:integer;  //* Count of output words pushed within current scheme chain */
   reason:pByte;                  //* Reason for status */
   sizeIdx:integer;               //* Symbol size of completed stream */
   status:DmtxStatus ;
   input:pDmtxByteList ;
   output:pDmtxByteList ;
end;

PDmtxImage=^DmtxImage;
PPDmtxImage=^PDmtxImage;
DmtxImage = record
   width:integer;
   height:integer;
   pixelPacking:integer;
   bitsPerPixel:integer;
   bytesPerPixel:integer;
   rowPadBytes:integer;
   rowSizeBytes:integer;
   imageFlip:integer;
   channelCount:integer;
   channelStart:array[0..3] of integer;
   bitsPerChannel:array[0..3] of integer;
   pxl: pByte;
end;

DmtxPointFlow = record
  plane:integer;
  arrive:integer;
  depart:integer;
  mag:integer;
  loc: DmtxPixelLoc;
end;

DmtxBestLine = record
  angle:integer;
  hOffset:integer;
  mag:integer;
  stepBeg:integer;
  stepPos:integer;
  stepNeg:integer;
  distSq:integer;
  devn:double;
  locBeg:DmtxPixelLoc;
  locPos:DmtxPixelLoc;
  locNeg:DmtxPixelLoc;
end;

ppDmtxRegion=^pDmtxRegion;
pDmtxRegion=^DmtxRegion;
DmtxRegion= record
  //* Trail blazing values */
  jumpToPos:integer;
  jumpToNeg:integer;
  stepsTotal:integer;
  finalPos:DmtxPixelLoc;
  finalNeg:DmtxPixelLoc;
  boundMin:DmtxPixelLoc;
  boundMax:DmtxPixelLoc;
  flowBegin:DmtxPointFlow;

  //* Orientation values */
  polarity:integer;
  stepR:integer;
  stepT:integer;
  locR:DmtxPixelLoc;          //* remove if stepR works above */
  locT:DmtxPixelLoc;          //* remove if stepT works above */

  //* Region fitting values */
  leftKnown:integer;     //* known == 1; unknown == 0 */
  leftAngle:integer;     //* hough angle of left edge */
  leftLoc:DmtxPixelLoc;  //* known (arbitrary) location on left edge */
  leftLine:DmtxBestLine;
  bottomKnown:integer;   //* known == 1; unknown == 0 */
  bottomAngle:integer;   //* hough angle of bottom edge */
  bottomLoc:DmtxPixelLoc;  //* known (arbitrary) location on bottom edge */
  bottomLine:DmtxBestLine;
  topKnown:integer;      //* known == 1; unknown == 0 */
  topAngle:integer;      //* hough angle of top edge */
  topLoc:DmtxPixelLoc;   //* known (arbitrary) location on top edge */
  rightKnown:integer;    //* known == 1; unknown == 0 */
  rightAngle:integer;    //* hough angle of right edge */
  rightLoc:DmtxPixelLoc;  //* known (arbitrary) location on right edge */

  //* Region calibration values */
  onColor:integer;
  offColor:integer;
  sizeIdx:integer;       //* Index of arrays that store Data Matrix constants */
  symbolRows:integer;    //* Number of total rows in symbol including alignment patterns */
  symbolCols:integer;    //* Number of total columns in symbol including alignment patterns */
  mappingRows:integer;   //* Number of data rows in symbol */
  mappingCols:integer;   //* Number of data columns in symbol */

  //* Transform values */
  raw2fit:DmtxMatrix3 ;   //* 3x3 transformation from raw image to fitted barcode grid */
  fit2raw:DmtxMatrix3 ;   //* 3x3 transformation from fitted barcode grid to raw image */
end;

ppDmtxMessage=^pDmtxMessage;
pDmtxMessage=^DmtxMessage;
DmtxMessage = record
   arraySize   :cardinal;      //* mappingRows * mappingCols */
   codeSize    :cardinal;      //* Size of encoded data (data words + error words) */
   outputSize  :cardinal;      //* Size of buffer used to hold decoded data */
   outputIdx   :integer;          //* Internal index used to store output progress */
   padCount    :integer;
   _array      :pbyte;         //* Pointer to internal representation of Data Matrix modules */
   code        :pbyte;         //* Pointer to internal storage of code words (data and error) */
   output      :pbyte;         //* Pointer to internal storage of decoded output */
end;
(*/**
 * @struct DmtxScanGrid
 * @brief DmtxScanGrid
 */
typedef struct DmtxScanGrid_struct {
   /* set once */
   int             minExtent;     /* Smallest cross size used in scan */
   int             maxExtent;     /* Size of bounding grid region (2^N - 1) */
   int             xOffset;       /* Offset to obtain image X coordinate */
   int             yOffset;       /* Offset to obtain image Y coordinate */
   int             xMin;          /* Minimum X in image coordinate system */
   int             xMax;          /* Maximum X in image coordinate system */
   int             yMin;          /* Minimum Y in image coordinate system */
   int             yMax;          /* Maximum Y in image coordinate system */

   /* reset for each level */
   int             total;         /* Total number of crosses at this size */
   int             extent;        /* Length/width of cross in pixels */
   int             jumpSize;      /* Distance in pixels between cross centers */
   int             pixelTotal;    /* Total pixel count within an individual cross path */
   int             startPos;      /* X and Y coordinate of first cross center in pattern */

   /* reset for each cross */
   int             pixelCount;    /* Progress (pixel count) within current cross pattern */
   int             xCenter;       /* X center of current cross pattern */
   int             yCenter;       /* Y center of current cross pattern */
} DmtxScanGrid;*)

(*/**
 * @struct DmtxTime
 * @brief DmtxTime
 */
typedef struct DmtxTime_struct {
   time_t          sec;
   unsigned long   usec;
} DmtxTime;*)
pDmtxTime=^DmtxTime;
ppDmtxTime=^pDmtxTime;
DmtxTime=record
  sec: longint; // for 32-bit systems. Int64 on 64-bit system
  usec: cardinal;
end;

pDmtxDecode=^DmtxDecode;
ppDmtxDecode=^pDmtxDecode;
DmtxDecode=record
  edgeMin:integer;
  edgeMax:integer;
  scanGap:integer;
  squareDevn:double;
  sizeIdxExpected:integer;
  edgeThresh:integer;

  //* Image modifiers */
  xMin:integer;
  xMax:integer;
  yMin:integer;
  yMax:integer;
  scale:integer;

  //* Internals */
  cache:pByte;
  image:pDmtxImage;
  //TODO: DmtxScanGrid    grid;
end;

pDmtxEncode = ^DmtxEncode;
ppDmtxEncode = ^pDmtxEncode;
DmtxEncode = record
  method         :integer;
  scheme         :integer;
  sizeIdxRequest :integer;
  marginSize     :integer;
  moduleSize     :integer;
  pixelPacking   :integer;
  imageFlip      :integer;
  rowPadBytes    :integer;
  message        :pDmtxMessage;
  image          :PDmtxImage;
  region         :DmtxRegion;
  xfrm           :DmtxMatrix3;   //* XXX still necessary? */
  rxfrm          :DmtxMatrix3;   //* XXX still necessary? */
end ;

(*/**
 * @struct DmtxChannel
 * @brief DmtxChannel
 */
typedef struct DmtxChannel_struct {
   int             encScheme;     /* current encodation scheme */
   int             invalid;       /* channel status (invalid if non-zero) */
   unsigned char  *inputPtr;      /* pointer to current input character */
   unsigned char  *inputStop;     /* pointer to position after final input character */
   int             encodedLength; /* encoded length (units of 2/3 bits) */
   int             currentLength; /* current length (units of 2/3 bits) */
   int             firstCodeWord; /* */
   unsigned char   encodedWords[1558];
} DmtxChannel;*)

(*/* Wrap in a struct for fast copies */
/**
 * @struct DmtxChannelGroup
 * @brief DmtxChannelGroup
 */
typedef struct DmtxChannelGroup_struct {
   DmtxChannel channel[6];
} DmtxChannelGroup;*)

(*/**
 * @struct DmtxTriplet
 * @brief DmtxTriplet
 */
typedef struct DmtxTriplet_struct {
   unsigned char   value[3];
} DmtxTriplet;*)

(*/**
 * @struct DmtxQuadruplet
 * @brief DmtxQuadruplet
 */
typedef struct DmtxQuadruplet_struct {
   unsigned char   value[4];
} DmtxQuadruplet;*)
var
//* dmtxtime.c */
//extern DmtxTime *dmtxTimeNow(void);
dmtxTimeNow: function:pDmtxTime;cdecl;
//extern DmtxPassFail dmtxTimeDestroy(DmtxTime **t);
dmtxTimeDestroy: function(t:ppDmtxTime):integer;cdecl;
//extern DmtxPassFail dmtxTimeAdd(DmtxTime *t, long msec);
dmtxTimeAdd: function(t:pDmtxTime;msec:integer):integer;cdecl;
//extern int dmtxTimeExceeded(DmtxTime *timeout);*)
dmtxTimeExceeded: function(timeout:pDmtxTime):integer;cdecl;

//* dmtxencode.c */
//extern DmtxEncode *dmtxEncodeCreate(void);
dmtxEncodeCreate: function:pDmtxEncode;cdecl;
//extern DmtxPassFail dmtxEncodeDestroy(DmtxEncode **enc); //DmtxPassFail=unsigned int
dmtxEncodeDestroy:function(enc:ppDmtxEncode):cardinal;cdecl;
//extern DmtxPassFail dmtxEncodeSetProp(DmtxEncode *enc, int prop, int value);
dmtxEncodeSetProp:function(enc:pDmtxEncode;prop:DmtxProperty;value:integer):cardinal;cdecl;
//extern int dmtxEncodeGetProp(DmtxEncode *enc, int prop);
dmtxEncodeGetProp:function(enc:pDmtxEncode;prop:integer):integer;cdecl;
//extern DmtxPassFail dmtxEncodeDataMatrix(DmtxEncode *enc, int n, unsigned char *s);
dmtxEncodeDataMatrix:function(enc:pDmtxEncode;n:integer;s:pByte):cardinal;cdecl;
//extern DmtxPassFail dmtxEncodeDataMosaic(DmtxEncode *enc, int n, unsigned char *s);
dmtxEncodeDataMosaic:function(enc:pDmtxEncode;n:integer;s:pByte):cardinal;cdecl;

///* dmtxdecode.c */
//extern DmtxDecode *dmtxDecodeCreate(DmtxImage *img, int scale);
dmtxDecodeCreate:function(img:pDmtxImage;scale:integer):pDmtxDecode;cdecl;
//extern DmtxPassFail dmtxDecodeDestroy(DmtxDecode **dec);
dmtxDecodeDestroy:function(dec: ppDmtxDecode):cardinal;cdecl;
//extern DmtxPassFail dmtxDecodeSetProp(DmtxDecode *dec, int prop, int value);
dmtxDecodeSetProp:function(dec:pDmtxDecode;prop:DmtxProperty;value:integer):cardinal;cdecl;
//extern int dmtxDecodeGetProp(DmtxDecode *dec, int prop);
//extern /*@exposed@*/ unsigned char *dmtxDecodeGetCache(DmtxDecode *dec, int x, int y);
//extern DmtxPassFail dmtxDecodeGetPixelValue(DmtxDecode *dec, int x, int y, int channel, /*@out@*/ int *value);
//extern DmtxMessage *dmtxDecodeMatrixRegion(DmtxDecode *dec, DmtxRegion *reg, int fix);
dmtxDecodeMatrixRegion:function(dec:pDmtxDecode; reg:pDmtxRegion; fix:integer):pDmtxMessage;cdecl;
//extern DmtxMessage *dmtxDecodeMosaicRegion(DmtxDecode *dec, DmtxRegion *reg, int fix);
dmtxDecodeMosaicRegion:function(dec:pDmtxDecode; reg:pDmtxRegion; fix:integer):pDmtxMessage;cdecl;
//extern unsigned char *dmtxDecodeCreateDiagnostic(DmtxDecode *dec, /*@out@*/ int *totalBytes, /*@out@*/ int *headerBytes, int style);


//* dmtxregion.c */
//extern DmtxRegion *dmtxRegionCreate(DmtxRegion *reg);
//extern DmtxPassFail dmtxRegionDestroy(DmtxRegion **reg);
dmtxRegionDestroy:function(reg: pDmtxRegion):cardinal;cdecl;
//extern DmtxRegion *dmtxRegionFindNext(DmtxDecode *dec, DmtxTime *timeout);
dmtxRegionFindNext:function(dec: pDmtxDecode; timeout:pDmtxTime):pDmtxRegion;cdecl;
//extern DmtxRegion *dmtxRegionScanPixel(DmtxDecode *dec, int x, int y);
//extern DmtxPassFail dmtxRegionUpdateCorners(DmtxDecode *dec, DmtxRegion *reg, DmtxVector2 p00,
//      DmtxVector2 p10, DmtxVector2 p11, DmtxVector2 p01);
//extern DmtxPassFail dmtxRegionUpdateXfrms(DmtxDecode *dec, DmtxRegion *reg);

//* dmtxmessage.c */
//extern DmtxMessage *dmtxMessageCreate(int sizeIdx, int symbolFormat);
//extern DmtxPassFail dmtxMessageDestroy(DmtxMessage **msg);
dmtxMessageDestroy:function(msg:ppDmtxMessage ):cardinal;cdecl;

///* dmtximage.c */
//extern DmtxImage *dmtxImageCreate(unsigned char *pxl, int width, int height, int pack);
dmtxImageCreate:function(pxl:pByte; width, height:integer; pack : DmtxPackOrder):pDmtxImage;cdecl;
//extern DmtxPassFail dmtxImageDestroy(DmtxImage **img);
dmtxImageDestroy:function(img:ppDmtxImage):cardinal;cdecl;
//extern DmtxPassFail dmtxImageSetChannel(DmtxImage *img, int channelStart, int bitsPerChannel);
//extern DmtxPassFail dmtxImageSetProp(DmtxImage *img, int prop, int value);
dmtxImageSetProp:function(img:pDmtxImage;prop:DmtxProperty;value:integer):cardinal;cdecl;
//extern int dmtxImageGetProp(DmtxImage *img, int prop);
//extern int dmtxImageGetByteOffset(DmtxImage *img, int x, int y);
//extern DmtxPassFail dmtxImageGetPixelValue(DmtxImage *img, int x, int y, int channel, /*@out@*/ int *value);
//extern DmtxPassFail dmtxImageSetPixelValue(DmtxImage *img, int x, int y, int channel, int value);
//extern DmtxBoolean dmtxImageContainsInt(DmtxImage *img, int margin, int x, int y);
//extern DmtxBoolean dmtxImageContainsFloat(DmtxImage *img, double x, double y);

(*
/* dmtxvector2.c */
extern DmtxVector2 *dmtxVector2AddTo(DmtxVector2 *v1, const DmtxVector2 *v2);
extern DmtxVector2 *dmtxVector2Add(/*@out@*/ DmtxVector2 *vOut, const DmtxVector2 *v1, const DmtxVector2 *v2);
extern DmtxVector2 *dmtxVector2SubFrom(DmtxVector2 *v1, const DmtxVector2 *v2);
extern DmtxVector2 *dmtxVector2Sub(/*@out@*/ DmtxVector2 *vOut, const DmtxVector2 *v1, const DmtxVector2 *v2);
extern DmtxVector2 *dmtxVector2ScaleBy(DmtxVector2 *v, double s);
extern DmtxVector2 *dmtxVector2Scale(/*@out@*/ DmtxVector2 *vOut, const DmtxVector2 *v, double s);
extern double dmtxVector2Cross(const DmtxVector2 *v1, const DmtxVector2 *v2);
extern double dmtxVector2Norm(DmtxVector2 *v);
extern double dmtxVector2Dot(const DmtxVector2 *v1, const DmtxVector2 *v2);
extern double dmtxVector2Mag(const DmtxVector2 *v);
extern double dmtxDistanceFromRay2(const DmtxRay2 *r, const DmtxVector2 *q);
extern double dmtxDistanceAlongRay2(const DmtxRay2 *r, const DmtxVector2 *q);
extern DmtxPassFail dmtxRay2Intersect(/*@out@*/ DmtxVector2 *point, const DmtxRay2 *p0, const DmtxRay2 *p1);
extern DmtxPassFail dmtxPointAlongRay2(/*@out@*/ DmtxVector2 *point, const DmtxRay2 *r, double t);
*)
(*
/* dmtxmatrix3.c */
extern void dmtxMatrix3Copy(/*@out@*/ DmtxMatrix3 m0, DmtxMatrix3 m1);
extern void dmtxMatrix3Identity(/*@out@*/ DmtxMatrix3 m);
extern void dmtxMatrix3Translate(/*@out@*/ DmtxMatrix3 m, double tx, double ty);
extern void dmtxMatrix3Rotate(/*@out@*/ DmtxMatrix3 m, double angle);
extern void dmtxMatrix3Scale(/*@out@*/ DmtxMatrix3 m, double sx, double sy);
extern void dmtxMatrix3Shear(/*@out@*/ DmtxMatrix3 m, double shx, double shy);
extern void dmtxMatrix3LineSkewTop(/*@out@*/ DmtxMatrix3 m, double b0, double b1, double sz);
extern void dmtxMatrix3LineSkewTopInv(/*@out@*/ DmtxMatrix3 m, double b0, double b1, double sz);
extern void dmtxMatrix3LineSkewSide(/*@out@*/ DmtxMatrix3 m, double b0, double b1, double sz);
extern void dmtxMatrix3LineSkewSideInv(/*@out@*/ DmtxMatrix3 m, double b0, double b1, double sz);
extern void dmtxMatrix3Multiply(/*@out@*/ DmtxMatrix3 mOut, DmtxMatrix3 m0, DmtxMatrix3 m1);
extern void dmtxMatrix3MultiplyBy(DmtxMatrix3 m0, DmtxMatrix3 m1);
extern int dmtxMatrix3VMultiply(/*@out@*/ DmtxVector2 *vOut, DmtxVector2 *vIn, DmtxMatrix3 m);
extern int dmtxMatrix3VMultiplyBy(DmtxVector2 *v, DmtxMatrix3 m);
extern void dmtxMatrix3Print(DmtxMatrix3 m);
*)

//* dmtxsymbol.c */
//extern int dmtxSymbolModuleStatus(DmtxMessage *mapping, int sizeIdx, int row, int col);
//extern int dmtxGetSymbolAttribute(int attribute, int sizeIdx);
dmtxGetSymbolAttribute:function(attribute:integer;sizeIdx:integer):integer;cdecl;
//extern int dmtxGetBlockDataSize(int sizeIdx, int blockIdx);

(*
/* dmtxbytelist.c */
extern DmtxByteList dmtxByteListBuild(DmtxByte *storage, int capacity);
extern void dmtxByteListInit(DmtxByteList *list, int length, DmtxByte value, DmtxPassFail *passFail);
extern void dmtxByteListClear(DmtxByteList *list);
extern DmtxBoolean dmtxByteListHasCapacity(DmtxByteList *list);
extern void dmtxByteListCopy(DmtxByteList *dst, const DmtxByteList *src, DmtxPassFail *passFail);
extern void dmtxByteListPush(DmtxByteList *list, DmtxByte value, DmtxPassFail *passFail);
extern DmtxByte dmtxByteListPop(DmtxByteList *list, DmtxPassFail *passFail);
extern void dmtxByteListPrint(DmtxByteList *list, char *prefix);
*)

//extern char *dmtxVersion(void);
dmtxDllVersion:function:pByte;
//#ifdef __cplusplus
//}
//#endif

//#endif

var
  dmtxDLLLoaded: Boolean = False;
implementation
var
  SaveExit: pointer;
  DLLHandle: THandle;
  ErrorMode: Integer;

  procedure NewExit; far;
  begin
    ExitProc := SaveExit;
    FreeLibrary(DLLHandle)
  end {NewExit};

procedure LoadDmtxDLL;
begin
  if dmtxDLLLoaded then Exit;
  ErrorMode := SetErrorMode($8000{SEM_NoOpenFileErrorBox});
  DLLHandle := LoadLibrary('dmtx.dll');
  if DLLHandle >= 32 then
  begin
    dmtxDLLLoaded := True;
    SaveExit := ExitProc;
    ExitProc := @NewExit;
(* S+R template for method entrypoints
    @dmtx := GetProcAddress(DLLHandle,'dmtx');
    Assert(@dmtx <> nil);

*)
    @dmtxDllVersion :=GetProcAddress(DLLHandle,'dmtxVersion');
    Assert(@dmtxDllVersion <> nil);

    // dmtxtime
    @dmtxTimeNow := GetProcAddress(DLLHandle,'dmtxTimeNow');
    Assert(@dmtxTimeNow <> nil);

    @dmtxTimeDestroy := GetProcAddress(DLLHandle,'dmtxTimeDestroy');
    Assert(@dmtxTimeDestroy <> nil);

    @dmtxTimeAdd:= GetProcAddress(DLLHandle,'dmtxTimeAdd');
    Assert(@dmtxTimeAdd <> nil);

    @dmtxTimeExceeded:= GetProcAddress(DLLHandle,'dmtxTimeExceeded');
    Assert(@dmtxTimeExceeded <> nil);

    // dmtxencode
    @dmtxEncodeCreate := GetProcAddress(DLLHandle,'dmtxEncodeCreate');
    Assert(@dmtxEncodeCreate <> nil);

    @dmtxEncodeDestroy := GetProcAddress(DLLHandle,'dmtxEncodeDestroy');
    Assert(@dmtxEncodeDestroy <> nil);

    @dmtxEncodeSetProp := GetProcAddress(DLLHandle,'dmtxEncodeSetProp');
    Assert(@dmtxEncodeSetProp <> nil);

    @dmtxEncodeGetProp := GetProcAddress(DLLHandle,'dmtxEncodeGetProp');
    Assert(@dmtxEncodeGetProp <> nil);

    @dmtxEncodeDataMatrix := GetProcAddress(DLLHandle,'dmtxEncodeDataMatrix');
    Assert(@dmtxEncodeDataMatrix <> nil);

    @dmtxEncodeDataMosaic := GetProcAddress(DLLHandle,'dmtxEncodeDataMosaic');
    Assert(@dmtxEncodeDataMosaic <> nil);


    @dmtxDecodeCreate := GetProcAddress(DLLHandle,'dmtxDecodeCreate');
    Assert(@dmtxDecodeCreate <> nil);

    @dmtxDecodeDestroy := GetProcAddress(DLLHandle,'dmtxDecodeDestroy');
    Assert(@dmtxDecodeDestroy <> nil);

    @dmtxDecodeSetProp := GetProcAddress(DLLHandle,'dmtxDecodeSetProp');
    Assert(@dmtxDecodeSetProp <> nil);

    @dmtxDecodeMatrixRegion := GetProcAddress(DLLHandle,'dmtxDecodeMatrixRegion');
    Assert(@dmtxDecodeMatrixRegion <> nil);

    @dmtxDecodeMosaicRegion := GetProcAddress(DLLHandle,'dmtxDecodeMosaicRegion');
    Assert(@dmtxDecodeMosaicRegion <> nil);

    @dmtxRegionDestroy := GetProcAddress(DLLHandle,'dmtxRegionDestroy');
    Assert(@dmtxRegionDestroy <> nil);

    @dmtxRegionFindNext := GetProcAddress(DLLHandle,'dmtxRegionFindNext');
    Assert(@dmtxRegionFindNext <> nil);

    @dmtxImageCreate := GetProcAddress(DLLHandle,'dmtxImageCreate');
    Assert(@dmtxImageCreate <> nil);

    @dmtxImageDestroy := GetProcAddress(DLLHandle,'dmtxImageDestroy');
    Assert(@dmtxImageDestroy <> nil);

    @dmtxImageSetProp := GetProcAddress(DLLHandle,'dmtxImageSetProp');
    Assert(@dmtxImageSetProp <> nil);

    @dmtxGetSymbolAttribute := GetProcAddress(DLLHandle,'dmtxGetSymbolAttribute');
    Assert(@dmtxGetSymbolAttribute <> nil);

    @dmtxMessageDestroy := GetProcAddress(DLLHandle,'dmtxMessageDestroy');
    Assert(@dmtxMessageDestroy <> nil);


  end
  else
  begin
    dmtxDLLLoaded := False;
  end;
  SetErrorMode(ErrorMode)
end {LoadDLL};

begin
  LoadDmtxDLL;
end.

