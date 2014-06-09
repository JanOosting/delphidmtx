unit DataMatrixBarcode;
{
Delphi specific part of libdmtx integration
Copyright (c) 2008 Jan Oosting

Implementation of the dmtxread and dmtxscan functionality from the utils directory of libdmtx
dmtxread,dmtxscan Copyright (c) 2008 Mike Laughton

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

Contact: j.oosting@lumc.nl
Jan 2008: start with encoding functionality
Apr/May 2008: adapted to version 0.5.0 of dmtxwrite and dmtxread
Sep 2012: Adapted to version 0.7.4 of libdmtx, dmtxwrite and dmtxread
}
interface
uses SysUtils, Windows, Graphics, dmtx, Classes;


const
  dmSymbolSize : array[-3..29] of string = ('Auto Rectangular','Auto Square','Auto Shape',
                   '10x10','12x12','14x14','16x16','18x18','20x20','22x22','24x24',
                   '26x26','32x32','36x36','40x40','44x44','48x48','52x52','64x64',
                   '72x72','80x80','88x88','96x96','104x104','120x120','132x132','144x144',
                   '8x18','8x32','12x26','12x36','16x36','16x48');

type
  // Errors are raised as exceptions
  EDataMatrixException = Class(Exception);

  // Options for Encoding
  DatamatrixEncodeOptions = record
    codewords:integer;
    moduleSize: integer;         // size of modules in pixels
    marginSize:integer;          // size of margin in pixels
    scheme: DmtxScheme;          // DmtxSchemeAutoFast,DmtxSchemeAutoBest,DmtxSchemeAscii,
                                 // DmtxSchemeC40,DmtxSchemeText,DmtxSchemeX12,
                                 // DmtxSchemeEdifact, DmtxSchemeBase256,DmtxSchemeEncodeAscii,
                                 // DmtxSchemeEncodeC40, DmtxSchemeEncodeText,
    rotate:integer;              // rotation angle (0,90,180,270 degrees)
    sizeIdx:DmtxSymbolSize;      // index into dmSymbolSize constant
    verbose:boolean;             // Not implemented: not appropriate in GUI
    mosaic:integer;              // create mosaic barcode (is buggy)
    color : array[0..2] of integer;
    bgcolor: array[0..2] of integer;
    dpi:integer;
  end;

  // Options for decoding
  DatamatrixDecodeOptions = record
    codewords:integer;           // Not implemented: provide all codewords in barcode
    scanGap:integer;             // gap between lines that scan for possible barcodes
    newline:integer;             // Not implemented: not appropriate in GUI
    xRangeMin:integer;           // Do not scan pixels to left of
    xRangeMax:integer;           // Do not scan pixels to right of
    yRangeMin:integer;           // Do not scan pixels above
    yRangeMax:integer;           // Do not scan pixels below
    verbose:integer;             // Not implemneted: not appropriate in GUI
    corners:integer;             // Not implemented: provide locations of corners of barcode
    diagnose:integer;            // Not implemented: make copy of image with added diagnostic data
    fix_errors:integer;          // apply ECC error correction
    mosaic:integer;              // interpret detected regions as Data Mosaic barcodes
    pageNumber:integer;          // Not implemented: prefix decoded message with fax/tiff page number
  end;


// Set an Options record to default values
function InitializeDatamatrixEncodeOptions:DatamatrixEncodeOptions;

// Encode a datamatrix barcode with default options
procedure EncodeDatamatrix(code:string;bitmap:TBitmap);overload;
  // code : any non-zero-length string
  // bitmap: an existing bitmap object, which is replaced by a barcode bitmap

// Encode a datamatrix barcode with custom options
procedure EncodeDatamatrix(code:string;bitmap:TBitmap;options:DatamatrixEncodeOptions);overload;
  // // the normal program flow here looks like
  // myOptions:= InitializeDatamatrixEncodeOptions;
  // //  set some options
  // myOptions.ModuleSize:=8;
  // myOptions.MarginSize:=4;
  // EncodeDatamatrix(myCode,myBitmap,myOptions);

// Set an Options record to default values
function InitializeDatamatrixDecodeOptions:DatamatrixDecodeOptions;

// Decode a datamatrix barcode with default options
procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings);overload;
  // bitmap: a bitmap that can contain a datamatrix barcode
  // codes: an existing TStrings descendent (TStringList, Items property in a component)
  //   currently recognition is stopped after one code is recognized

// Decode a datamatrix barcode with custom options
procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings;options:DatamatrixDecodeOptions);overload;
// options can be set analogous to EncodeDatamatrix with options

procedure RotateQuartiles(angle:integer;bitmap:TBitmap);

implementation
const bufferSize=4096;

type
  TRGBArray    = ARRAY[0..4095] OF TRGBTriple;
  pRGBArray    = ^TRGBArray;

function InitializeDatamatrixEncodeOptions:DatamatrixEncodeOptions;
var
  I: Integer;
begin
  result.codewords:=DmtxFalse;
  result.moduleSize:=5;
  result.marginSize:=10;
  result.scheme:=DmtxSchemeAutoBest;
  result.rotate:=0;
  result.sizeIdx:=DmtxSymbolSquareAuto;
  for I := 0 to 2 do
  begin
    result.color[i]:=0;
    result.bgcolor[i]:=255;
  end;
  result.dpi:= DmtxUndefined;
  result.mosaic:=DmtxFalse;
end;

procedure EncodeDatamatrix(code:string;bitmap:TBitmap);
begin
  EncodeDatamatrix(code,bitmap,InitializeDatamatrixEncodeOptions);
end;

procedure RotateQuartiles(angle:integer;bitmap:TBitmap);
var
  rotatedBitmap:TBitmap;
  i,j:integer;
  PixelSource,PixelDest : pRGBArray;

begin
  rotatedBitmap:=TBitmap.Create;
  try
    rotatedBitmap.PixelFormat:=bitmap.PixelFormat;
    if (angle=90) or (angle=270) then
    begin
      rotatedBitmap.Width:=bitmap.Height;
      rotatedBitmap.Height:=bitmap.Width;
    end
    else begin
      rotatedBitmap.Width:=bitmap.Width;
      rotatedBitmap.Height:=bitmap.Height;
    end;
    case angle of
    0:begin
      // just copy
      rotatedBitmap.Canvas.Draw(0,0,bitmap);
    end;
    90:begin
      for j := 0 to Bitmap.Height - 1 do
      begin
        PixelSource  := Bitmap.ScanLine[j];
        for i := 0 to Bitmap.Width - 1 do
          pRGBArray(rotatedBitmap.ScanLine[Bitmap.Width - i - 1])[j] := PixelSource[i]
      end;
    end;
    180:begin
      for  j := 0 to Bitmap.Height - 1 do
      begin
        PixelSource  := Bitmap.ScanLine[j];
        PixelDest := rotatedBitmap.ScanLine[Bitmap.Height - j - 1];
        for i := 0 to Bitmap.Width - 1 do
          pixelDest[Bitmap.Width - i - 1] := PixelSource[i]
      end;
    end;
    270:begin
      for  j := 0 to Bitmap.Height - 1 do
      begin
        PixelSource  := Bitmap.ScanLine[j];
        for i := 0 to Bitmap.Width - 1 do
          pRGBARray(rotatedBitmap.Scanline[i])[Bitmap.Height - j - 1] := PixelSource[i]
      end;

    end;
    else
      raise EDataMatrixException.Create('Only 0, 90, 180, and 270 are allowed for rotation');
    end;
    bitmap.Assign(rotatedBitmap);
  finally
    rotatedBitmap.Free;
  end;
end;

procedure EncodeDatamatrix(code:string;bitmap:TBitmap;options:DatamatrixEncodeOptions);
var
  codebuffer:array[0..bufferSize] of AnsiChar;
  encode : pDmtxEncode;
  row, rowsize:cardinal;
  source,destination:pointer;
begin
  if length(code)=0 then
    Raise EDataMatrixException.Create('The code should contain at least 1 character');
  encode:=dmtxEncodeCreate();
  try
  //* Set output image properties */
  dmtxEncodeSetProp(encode, DmtxPropPixelPacking, integer(DmtxPack24bppRGB));
  dmtxEncodeSetProp(encode, DmtxPropImageFlip, integer(DmtxFlipNone));
  dmtxEncodeSetProp(encode, DmtxPropRowPadBytes, 0);

  //* Set encoding options */
  dmtxEncodeSetProp(encode, DmtxPropMarginSize, options.marginSize);
  dmtxEncodeSetProp(encode, DmtxPropModuleSize, options.moduleSize);
  dmtxEncodeSetProp(encode, DmtxPropScheme, integer(options.scheme));
  dmtxEncodeSetProp(encode, DmtxPropSizeRequest, integer(options.sizeIdx));

  StrPCopy(PansiChar(@codebuffer), AnsiString(Copy(code,1,buffersize-1)));
  if options.mosaic = DmtxTrue then
  begin
    if dmtxEncodeDataMosaic(encode, length(code), @codebuffer)=DmtxFail then
      raise EDataMatrixException.Create('Error processing code');
  end
  else begin
    if dmtxEncodeDataMatrix(encode, length(code), @codebuffer)=DmtxFail then
      raise EDataMatrixException.Create('Error processing code');
  end;

  bitmap.width:=0;
  bitmap.PixelFormat:=pf24Bit;
  bitmap.Height:=encode.image.height;
  bitmap.Width:=encode.image.width;
  rowsize:=encode.image.rowSizeBytes+encode.image.rowPadBytes;

  for row := 0 to encode.image.height - 1 do
  begin
    source:=pointer(cardinal(encode.image.pxl)+row*rowsize);
    destination:=bitmap.ScanLine[encode.image.height-row-1];
    Move(source^,destination^,rowsize);
  end;
  if options.rotate<>0 then
    RotateQuartiles(options.rotate,bitmap);
  finally
  dmtxEncodeDestroy(@encode);

  end;
end;

function InitializeDatamatrixDecodeOptions:DatamatrixDecodeOptions;
begin
  result.codewords := 0;
  result.scanGap := 5;
  result.newline := 0;
  result.xRangeMin := -1;
  result.xRangeMax := -1;
  result.yRangeMin := -1;
  result.yRangeMax := -1;
  result.verbose := 0;
  result.corners := 0;
  result.diagnose:=0;
  result.fix_errors:=1;
  result.mosaic := 0;
  result.pageNumber := 0;
end;

procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings);overload;
begin
  DecodeDatamatrix(bitmap,codes,InitializeDatamatrixDecodeOptions);
end;

function DIBtoImage(bitmap:TBitmap):PDmtxImage;
var
  bm:TBitmap;
  rowsize:cardinal;
  row:cardinal;
  source,destination:pointer;
begin
  // make a copy with 24bit in order not to change the original
  bm:=TBitmap.Create;
  try
    bm.PixelFormat:=pf24bit;
    bm.Width:=bitmap.Width;
    bm.Height:=bitmap.Height;
    bm.Canvas.Draw(0,0,bitmap);
    //result:=dmtxImageMalloc(bm.Width,bm.Height);

    rowsize:=result.width*SizeOf(TRGBTriple);

    for row := 0 to result.height - 1 do
    begin
      //source:=bm.ScanLine[row];
      source:=bm.ScanLine[result.height-row-1];
      destination:=pointer(cardinal(result.pxl)+row*rowsize);
      Move(source^,destination^,rowsize);
    end;

  finally
    bm.Free;
  end;
end;

procedure SetScanRegion(var p0,p1:DmtxPixelLoc;var options:DatamatrixDecodeOptions;image:PDmtxImage);
var
  error:boolean;
  function checkRange(var target:integer; option:integer;minMax:integer;limit:integer):boolean;
  begin
    if option=-1 then
      target:=limit * minMax
    else
      target:=option;
    result:= (target<0) or (target>limit);
  end;

begin
  error:=checkRange(p0.X,options.xRangeMin,0,image.width-1) or
         checkRange(p0.Y,options.yRangeMin,0,image.height-1) or
         checkRange(p1.X,options.xRangeMax,1,image.width-1) or
         checkRange(p1.Y,options.yRangeMax,1,image.height-1);
  if error then
    raise EDataMatrixException.Create('Badly formed range parameter');
  if (p0.X >= p1.x) or (p0.Y >= p1.Y) then
    raise EDataMatrixException.Create('Specified range has non-positive area');
end;

(*procedure DecodeOutput(options:DatamatrixDecodeOptions; image: pDmtxImage;
    region:pDmtxRegion; _message: pDmtxmessage; pageIndex:integer; codes:TStrings);
//var
  //dataWordLength:integer;
  //rotateInt:integer;
  //rotate:double;

begin

  //dataWordLength := dmtxGetSymbolAttribute(DmtxSymAttribDataWordLength, region.sizeIdx);
  codes.Add(pChar(_message.output));
end;*)

procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings;options:DatamatrixDecodeOptions);overload;
var
  decode:DmtxDecode;
  image:PDmtxImage;
  region: DmtxRegion;
  p0,p1: DmtxPixelLoc;
  _message:pDmtxMessage;

begin
(*  codes.Clear;
  image:=DIBtoImage(bitmap);
  try
    if image.pageCount<1 then
      raise EDataMatrixException.Create('Error loading image');
    SetScanRegion(p0,p1,options,image);
    decode:=dmtxDecodeStructInit(image,p0,p1,options.scanGap);
    try
      region:=dmtxDecodeFindNextRegion(@decode);
      while region.found<> DMTX_REGION_EOF do
      begin
        _message:=dmtxDecodeMatrixRegion(@decode,@region,options.fix_errors);
        DecodeOutput(options,image,@region,_message,0,codes);
        dmtxMessageFree(@_message);
        break; // for now stop after first code
      end;
    finally
      dmtxDecodeStructDeInit(@decode);
    end;
  finally
    dmtxImageFree(@image);
  end;*)
end;

end.
