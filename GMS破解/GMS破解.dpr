program GMSÆÆ½â;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MyUtils in 'MyUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
