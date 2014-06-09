program TestDatamatrixDLL;

{%File 'TestDatamatrixDLL.bdsproj'}

uses
  Forms,
  main in 'main.pas' {frmMain},
  DataMatrixBarcode in 'DataMatrixBarcode.pas',
  dmtx in 'dmtx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
