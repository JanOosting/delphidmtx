unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataMatrixBarcode, dmtx, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs;
(*
Demo program using DataMatrixBarcode unit

Copyright (c) 2008 Jan Oosting

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

contact Jan Oosting:  j.oosting@lumc.nl
*)

type
  TfrmMain = class(TForm)
    pcMain: TPageControl;
    tsWrite: TTabSheet;
    Label1: TLabel;
    imgBarcode: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    btnCreateBarcode: TButton;
    UpDown1: TUpDown;
    edtModuleSize: TEdit;
    edtMarginSize: TEdit;
    UpDown2: TUpDown;
    cbScheme: TComboBox;
    cbMatrixSize: TComboBox;
    rgBarcodetype: TRadioGroup;
    memTekst: TMemo;
    btnCopy: TButton;
    tsRead: TTabSheet;
    ScrollBox1: TScrollBox;
    imgScan: TImage;
    btnLoadPicture: TButton;
    btnScan: TButton;
    memScanResults: TMemo;
    OpenPictureDialog: TOpenPictureDialog;
    Label8: TLabel;
    shpSelector: TShape;
    edtScanGap: TEdit;
    cbRotation: TComboBox;
    UpDown4: TUpDown;
    rgScanBarcodeType: TRadioGroup;
    procedure btnCopyClick(Sender: TObject);
    procedure btnCreateBarcodeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadPictureClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure imgScanMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScanMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgScanMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    CheckMainMovements:boolean;
    PrevX:integer;
    PrevY:integer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses Clipbrd;
{$R *.dfm}

procedure TfrmMain.btnCopyClick(Sender: TObject);
begin
  Clipboard.Assign(imgBarcode.Picture.Bitmap);
end;

procedure TfrmMain.btnCreateBarcodeClick(Sender: TObject);
var
  options:DatamatrixEncodeOptions;
begin
  options:=InitializeDatamatrixEncodeOptions;
  options.moduleSize:=StrToInt(edtModuleSize.Text);
  options.marginSize:=StrToInt(edtMarginSize.Text);
  options.rotate:=90*cbRotation.ItemIndex;
  options.scheme:=DmtxScheme(cbScheme.ItemIndex-2);
  options.sizeIdx:=DmtxSymbolSize(cbMatrixSize.ItemIndex-3);
  options.mosaic:=rgBarcodetype.ItemIndex;
  EncodeDatamatrix(memTekst.Text,imgBarcode.Picture.Bitmap,options);
end;

procedure TfrmMain.btnLoadPictureClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    imgScan.Picture.LoadFromFile(OpenPictureDialog.FileName);
    imgScan.Width:=imgScan.Picture.Width;
    imgScan.Height:=imgScan.Picture.Height;
    shpSelector.visible:=false;
  end;
end;

procedure TfrmMain.btnScanClick(Sender: TObject);
var
  decOptions:DatamatrixDecodeOptions;
begin
  memScanResults.Text:='<Scanning>';
  memScanResults.Update;
  if shpSelector.visible and
     (abs(shpSelector.Width)>5) and
     (abs(shpSelector.Height)>5) then
  begin
    decOptions:=InitializeDatamatrixDecodeOptions;
    if shpSelector.Width>0 then
    begin
      decoptions.xRangeMin:=shpSelector.Left;
      decOptions.xRangeMax:=shpSelector.Left+shpSelector.Width;
    end
    else begin
      decoptions.xRangeMin:=shpSelector.Left+shpSelector.Width;
      decOptions.xRangeMax:=shpSelector.Left;
    end;
    if shpSelector.Height>0 then
    begin
      decOptions.yRangeMin:=shpSelector.Top;
      decOptions.yRangeMax:=shpSelector.Top+shpSelector.Height;
    end
    else begin
      decOptions.yRangeMin:=shpSelector.Top+shpSelector.Height;
      decOptions.yRangeMax:=shpSelector.Top;
    end;
    DecodeDatamatrix(imgScan.Picture.Bitmap,memScanResults.Lines,decOptions);
  end
  else
    DecodeDatamatrix(imgScan.Picture.Bitmap,memScanResults.Lines);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pcMain.TabIndex:=0;
  memTekst.Text:='test';
  btnCreateBarcodeClick(Sender);
end;

procedure TfrmMain.imgScanMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PrevX:=X;
  PrevY:=Y;
  CheckMainMovements := True;

end;

procedure TfrmMain.imgScanMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if CheckMainMovements then
  begin
    shpSelector.Visible:=true;
    shpSelector.Top:=imgScan.Top+PrevY;
    shpSelector.Left:=imgScan.Left+PrevX;
    shpSelector.Height:=y-PrevY;
    shpSelector.Width:=x-PrevX;
  end;
end;

procedure TfrmMain.imgScanMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CheckMainMovements:=false;
end;

end.
