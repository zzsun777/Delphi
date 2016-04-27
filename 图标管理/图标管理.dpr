program Õº±Íπ‹¿Ì;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  CUtils in 'CUtils.pas',
  ArrayEx in 'ArrayEx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
