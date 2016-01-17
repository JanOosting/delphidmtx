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
uses SysUtils, Windows, Graphics, Math, dmtx, Classes, JPEG;


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
    edgeMin:integer;             //* -e, --minimum-edge */
    edgeMax:integer;             //* -E, --maximum-edge */
    scanGap:integer;             // gap between lines that scan for possible barcodes
    timeoutMS:integer;           //* -m, --milliseconds */
    newline:integer;             // Not implemented: not appropriate in GUI
    page:integer;                //* -p, --page */
    squareDevn:integer;          //* -q, --square-deviation */
    dpi:integer;                 //* -r, --resolution */
    sizeIdxExpected:integer;     //* -s, --symbol-size */
    edgeThresh:integer;          //* -t, --threshold */

    xMin:integer;           // Do not scan pixels to left of
    xMax:integer;           // Do not scan pixels to right of
    yMin:integer;           // Do not scan pixels above
    yMax:integer;           // Do not scan pixels below
    regionPercentage: boolean;  // use region values as percentage
    correctionsMax:integer;      //* -C, --corrections-max */
    shrinkMax:integer;           //* -S, --shrink */
    shrinkMin:integer;           //* -S, --shrink (if range specified) */
    unicode:integer;             //* -U, --unicode */
    stopAfter:integer;            //* -N, --stop-after */
    verbose:integer;             // Not implemneted: not appropriate in GUI
    corners:integer;             // Not implemented: provide locations of corners of barcode
    diagnose:integer;            // Not implemented: make copy of image with added diagnostic data
    fix_errors:integer;          // apply ECC error correction
    mosaic:integer;              // interpret detected regions as Data Mosaic barcodes
    pageNumbers:integer;          // Not implemented: prefix decoded message with fax/tiff page number
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

// Prepare a bitmap for finding barcodes
function DIBtoImage(bitmap:TBitmap):PDmtxImage;
// Decode a datamatrix barcode with default options
procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings);overload;
  // bitmap: a bitmap that can contain a datamatrix barcode
  // codes: an existing TStrings descendent (TStringList, Items property in a component)

// Decode a datamatrix barcode with custom options
procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings;options:DatamatrixDecodeOptions);overload;
// options can be set analogous to EncodeDatamatrix with options

procedure RotateQuartiles(angle:integer;bitmap:TBitmap);

Type
  TDataMatrixDecode = class(TObject)
  private
    fImage:PDmtxImage;
    fOptions:DatamatrixDecodeOptions;
    function GetStopafter: integer;
    function GettimeoutMS: integer;
    procedure SetStopAfter(const Value: integer);
    procedure SettimeoutMS(const Value: integer);
    function GetedgeMax: integer;
    function GetedgeMin: integer;
    function GetedgeThresh: integer;
    function GetScanGap: integer;
    function getsquareDevn: integer;
    procedure SetedgeMax(const Value: integer);
    procedure SetedgeMin(const Value: integer);
    procedure SetedgeThresh(const Value: integer);
    procedure SetScanGap(const Value: integer);
    procedure SetsquareDevn(const Value: integer);
  public
    constructor Create(bitmap:TBitmap);
    destructor Destroy;override;
    procedure Decode(codes:TStrings);
    procedure DecodeRegion(codes:TStrings;rect:TRect);
    property StopAfter:integer read GetStopafter write SetStopAfter;
    property timeoutMS:integer read GettimeoutMS write SettimeoutMS;    //* -m, --milliseconds */
    property scanGap:integer read GetScanGap write SetScanGap;          //* -g  --gap=N gap between lines that scan for possible barcodes
    property edgeMin:integer read GetedgeMin write SetedgeMin;          //* -e, --minimum-edge */
    property edgeMax:integer read GetedgeMax write SetedgeMax;          //* -E, --maximum-edge */
    property squareDevn:integer read getsquareDevn write SetsquareDevn; //* -q, --square-deviation */
    property edgeThresh:integer read GetedgeThresh write SetedgeThresh; //* -t, --threshold */
  end;

implementation
const bufferSize=4096;

type
  TRGBArray    = ARRAY[0..4095] OF TRGBTriple;
  pRGBArray    = ^TRGBArray;

// Utility functions
procedure ClearAndFreeObjects(strings:TStrings);
var
  I: Integer;
begin
  for I := 0 to strings.Count-1 do
  begin
    if assigned(strings.Objects[i]) then
      strings.Objects[i].Free;
  end;
  strings.Clear;
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
    destination:=GetMemory(bm.Width*bm.Height*SizeOf(TRGBTriple));
    result:=dmtxImageCreate(destination, bm.width, bm.height, DmtxPack24bppRGB);

    rowsize:=result.width*SizeOf(TRGBTriple);

    for row := 0 to result.height - 1 do
    begin
      source:=bm.ScanLine[row];
      //source:=bm.ScanLine[result.height-row-1];
      destination:=pointer(cardinal(result.pxl)+row*rowsize);
      Move(source^,destination^,rowsize);
    end;

  finally
    bm.Free;
  end;
end;

function ScaleNumber(value, size:integer;doScale:boolean):integer;
begin
  if doScale then
    result:= max(min(value * (size-1) div 100,size-1),0)
  else
    result:=value;
end;



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
    //destination:=bitmap.ScanLine[encode.image.height-row-1];
    destination:=bitmap.ScanLine[row];
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
   result.codewords := DmtxFalse;
   result.edgeMin := DmtxUndefined;
   result.edgeMax := DmtxUndefined;
   result.scanGap := 2;
   result.timeoutMS := DmtxUndefined;
   result.newline := DmtxFalse;
   result.page := DmtxUndefined;
   result.squareDevn := DmtxUndefined;
   result.dpi := DmtxUndefined;
   result.sizeIdxExpected := integer(DmtxSymbolShapeAuto);
   result.edgeThresh := 5;
   result.xMin := DmtxUndefined;
   result.xMax := DmtxUndefined;
   result.yMin := DmtxUndefined;
   result.yMax := DmtxUndefined;
   result.regionPercentage := false;
   result.correctionsMax := DmtxUndefined;
   result.diagnose := DmtxFalse;
   result.mosaic := DmtxFalse;
   result.stopAfter := DmtxUndefined;
   result.pageNumbers := DmtxFalse;
   result.corners := DmtxFalse;
   result.shrinkMin := 1;
   result.shrinkMax := 1;
   result.unicode := DmtxFalse;
   result.verbose := DmtxFalse;
end;

function SetDecodeOptions(decode:pDmtxDecode;image:pDmtxImage;options:DatamatrixDecodeOptions):boolean;
begin
  try
    if dmtxDecodeSetProp(decode, DmtxPropScanGap, options.scanGap)=DmtxFail then raise Exception.Create('Illegal value for scangap');

    if(options.edgeMin <> DmtxUndefined) then
      if dmtxDecodeSetProp(decode, DmtxPropEdgeMin, options.edgeMin)=DmtxFail then raise Exception.Create('Illegal value for edgeMin');

    if(options.edgeMax <> DmtxUndefined) then
       if dmtxDecodeSetProp(decode, DmtxPropEdgeMax, options.edgeMax)=DmtxFail then raise Exception.Create('Illegal value for edgeMax');

    if(options.squareDevn <> DmtxUndefined) then
      if dmtxDecodeSetProp(decode, DmtxPropSquareDevn, options.squareDevn)=DmtxFail then raise Exception.Create('Illegal value for squareDevn');

    if dmtxDecodeSetProp(decode, DmtxPropSymbolSize, options.sizeIdxExpected)=DmtxFail then raise Exception.Create('Illegal value for sizeIdxExpedted');

    if dmtxDecodeSetProp(decode, DmtxPropEdgeThresh, options.edgeThresh)=DmtxFail then raise Exception.Create('Illegal value for edgeThresh');

    if(options.xMin <> DmtxUndefined) then
      if dmtxDecodeSetProp(decode, DmtxPropXmin, ScaleNumber(options.xMin, image.width, options.regionPercentage))=DmtxFail then raise Exception.Create('Illegal value for xMin');

    if(options.xMax <> DmtxUndefined) then
      if dmtxDecodeSetProp(decode, DmtxPropXmax, ScaleNumber(options.xMax, image.width, options.regionPercentage))=DmtxFail then raise Exception.Create('Illegal value for xMax');

    if(options.yMin <> DmtxUndefined) then
      if dmtxDecodeSetProp(decode, DmtxPropYmin, ScaleNumber(options.yMin, image.height, options.regionPercentage))=DmtxFail then raise Exception.Create('Illegal value for yMin');

    if(options.yMax <> DmtxUndefined) then
      if dmtxDecodeSetProp(decode, DmtxPropYmax, ScaleNumber(options.yMax, image.height, options.regionPercentage))=DmtxFail then raise Exception.Create('Illegal value for yMax');
    result:=true;
  except
    result:=false;
  end;
end;

procedure DecodeMessage(msg:pDmtxMessage; region:pDMtxRegion; codes:TStrings;options:DatamatrixDecodeOptions; adjust:Tpoint);
var
  aRegion:pDmtxRegion;
begin
  new(aRegion);
  aRegion^:=region^;
  Inc(aRegion.boundMin.X,adjust.X);
  Inc(aRegion.boundMin.Y,adjust.Y);
  Inc(aRegion.boundMax.X,adjust.X);
  Inc(aRegion.boundMax.Y,adjust.Y);
  codes.AddObject(string(PAnsiChar(msg.output)),TObject(aRegion));
end;


procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings;options:DatamatrixDecodeOptions);overload;
var
  decode:pDmtxDecode;
  image:PDmtxImage;
  region: pDmtxRegion;
  msg:pDmtxMessage;
  timeout: pDmtxTime;

begin
  codes.Clear;
  image:=DIBtoImage(bitmap);
  dmtxImageSetProp(image, DmtxPropImageFlip, Integer(DmtxFlipNone));
  decode := dmtxDecodeCreate(image, 1);
  try
    if SetDecodeOptions(decode,image,options) then
    begin
      timeout:=nil;
      if options.timeoutMS>0 then
      begin
        timeout:=dmtxTimeNow;
        dmtxTimeAdd(timeout, options.timeoutMS);
      end;
      while true do
      begin
        region:= dmtxRegionFindNext(decode,timeout);
        if not assigned(region) then
          break;
        if options.mosaic = DmtxTrue then
          msg:= dmtxDecodeMosaicRegion(decode, region, options.correctionsMax)
        else
          msg:= dmtxDecodeMatrixRegion(decode, region, options.correctionsMax);
        if assigned(msg) then
        begin
          DecodeMessage(msg,region,codes,options,Point(0,0));
          dmtxMessageDestroy(@msg);
        end;
      end;
    end;
  finally
    FreeMemory(image.pxl);
    dmtxTimeDestroy(@timeout);
    dmtxImageDestroy(@image);
  end;
end;

procedure DecodeDatamatrix(bitmap:TBitmap;codes:TStrings);overload;
begin
  DecodeDatamatrix(bitmap,codes,InitializeDatamatrixDecodeOptions);
end;


{ TDataMatrixDecode }

constructor TDataMatrixDecode.Create(bitmap: TBitmap);
begin
  fImage:=DIBtoImage(bitmap);
  dmtxImageSetProp(fImage, DmtxPropImageFlip, Integer(DmtxFlipNone));
  fOptions:=InitializeDatamatrixDecodeOptions;
end;

procedure TDataMatrixDecode.Decode(codes: TStrings);
begin
  DecodeRegion(codes,Rect(0,0,fImage.width,fImage.height));
end;

procedure TDataMatrixDecode.DecodeRegion(codes: TStrings; rect: TRect);
var
  fDecode:pDmtxDecode;
  image:PDmtxImage;
  options:DatamatrixDecodeOptions;
  timeout : pDmtxTime;
  region: pDmtxRegion;
  msg:pDmtxMessage;
  codecount:integer;
  rectwidth, rectheight, destrowsize, row : cardinal;
  srcrowsize : cardinal;
  source, destination : pointer;
begin
  if rect.Right>0 then
  begin
    foptions.xMin:=rect.Left;
    fOptions.xMax:=rect.Left+rect.Right;
  end
  else begin
    foptions.xMin:=rect.Left+rect.Right;
    fOptions.xMax:=rect.Left;
  end;
  if rect.Bottom>0 then
  begin
    fOptions.yMin:=fIMage.Height-(rect.Top+rect.Bottom);
    fOptions.yMax:=fImage.Height-rect.Top;
  end
  else begin
    fOptions.yMin:=fImage.Height-rect.Top;
    fOptions.yMax:=fImage.Height-(rect.Top+rect.Bottom);
  end;
  codecount:=0;
  //
  Options:=fOptions;

  rectwidth := fOptions.xMax - fOptions.xMin;
  rectheight := fOptions.yMax - fOptions.yMin;
  destination:=GetMemory(rectwidth*rectheight*SizeOf(TRGBTriple));
  Image:=dmtxImageCreate(destination, rectwidth, rectheight, DmtxPack24bppRGB);
  destrowsize:=Image.width*SizeOf(TRGBTriple);
  srcrowsize:=fImage.width*SizeOf(TRGBTriple);
  for row := 0 to Image.height - 1 do
  begin
    source:=pointer(cardinal(fImage.pxl)+(Cardinal(fOptions.xMin)*Sizeof(TRGBTriple))+(row+cardinal(fImage.height-options.yMax))*srcrowsize);
    destination:=pointer(cardinal(Image.pxl)+row*destrowsize);
    Move(source^,destination^,destrowsize);
  end;
  options.xMin:=DmtxUndefined;
  options.xMax:=DmtxUndefined;
  options.yMin:=DmtxUndefined;
  options.yMax:=DmtxUndefined;

  fDecode:=dmtxDecodeCreate(Image, 1);
  if SetDecodeOptions(fDecode,Image,Options) then
  begin
    timeout:=nil;
    if Options.timeoutMS>0 then
    begin
      timeout:=dmtxTimeNow;
      dmtxTimeAdd(timeout,Options.timeoutMS);
    end;
    while (Options.stopAfter=DmtxUndefined) or (codecount<Options.stopAfter) do
    begin
      region:= dmtxRegionFindNext(fdecode,timeout);
      if not assigned(region) then
        break;
      if Options.mosaic = DmtxTrue then
        msg:= dmtxDecodeMosaicRegion(fDecode, region, Options.correctionsMax)
      else
        msg:= dmtxDecodeMatrixRegion(fDecode, region, Options.correctionsMax);
      if assigned(msg) then
      begin
        DecodeMessage(msg,region,codes,Options,Point(fOptions.xMin,fOptions.yMin));
        if (codes[codes.Count-1]<>'') then
          Inc(codecount);
        dmtxMessageDestroy(@msg);
      end;
    end;
    dmtxTimeDestroy(@timeout);
  end;
  FreeMemory(image.pxl);
  dmtxImageDestroy(@image);
  dmtxDecodeDestroy(@fDecode);
end;

destructor TDataMatrixDecode.Destroy;
begin
  FreeMemory(fImage.pxl);
  dmtxImageDestroy(@fImage);
  inherited;
end;

function TDataMatrixDecode.GetedgeMax: integer;
begin
  result:=fOptions.edgeMax;
end;

function TDataMatrixDecode.GetedgeMin: integer;
begin
  result:=fOptions.edgeMin;
end;

function TDataMatrixDecode.GetedgeThresh: integer;
begin
  result:=fOptions.edgeThresh;
end;

function TDataMatrixDecode.GetScanGap: integer;
begin
  Result:=fOptions.scanGap;
end;

function TDataMatrixDecode.getsquareDevn: integer;
begin
  Result:=fOptions.squareDevn;
end;

function TDataMatrixDecode.GetStopafter: integer;
begin
  result:=fOptions.stopAfter;
end;

function TDataMatrixDecode.GettimeoutMS: integer;
begin
  result:=fOptions.timeoutMS;
end;

procedure TDataMatrixDecode.SetedgeMax(const Value: integer);
begin
  fOptions.edgeMax:=value;
end;

procedure TDataMatrixDecode.SetedgeMin(const Value: integer);
begin
  fOptions.edgeMin:=Value;
end;

procedure TDataMatrixDecode.SetedgeThresh(const Value: integer);
begin
  fOptions.edgeThresh:=value;
end;

procedure TDataMatrixDecode.SetScanGap(const Value: integer);
begin
  fOptions.scanGap:=value;
end;

procedure TDataMatrixDecode.SetsquareDevn(const Value: integer);
begin
  fOptions.squareDevn:=value;
end;

procedure TDataMatrixDecode.SetStopAfter(const Value: integer);
begin
  fOptions.stopAfter:=Value;
end;

procedure TDataMatrixDecode.SettimeoutMS(const Value: integer);
begin
  fOptions.timeoutMS:=value;
end;

end.
