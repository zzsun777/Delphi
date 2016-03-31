unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    edt1: TEdit;
    btn1: TButton;
    btn_CRK: TButton;
    btn_RemovePWD: TButton;
    btn_Exit: TButton;
    dlgOpen1: TOpenDialog;
    procedure btn1Click(Sender: TObject);
    procedure btn_ExitClick(Sender: TObject);
    procedure btn_CRKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  if dlgOpen1.Execute(Self.Handle) then
  begin
    edt1.Text := dlgOpen1.FileName;
  end;
end;

procedure TForm1.btn_CRKClick(Sender: TObject);
var
  fileName: string;
  fs, fo: TFileStream;
  oldB, newB: TBytes;
  I: Integer;
  xors: WordBool;
begin
  fileName := edt1.Text;
  if FileExists(fileName) then
  begin
    try
      fs := TFileStream.Create(fileName, fmOpenRead);
      SetLength(oldB, fs.Size);
      SetLength(newB, fs.Size);
      fs.ReadBuffer(oldB, fs.Size);
      for I := 0 to fs.Size do
      begin
        if (oldB[i] = $2f) and (oldB[i + 1] = $30) and (oldB[i + 2] = $ee) and (oldB[i + 3] = $1f) then
        begin
          xors := True;
          break;
        end;
      end;
      if xors then
      begin
        RenameFile(fileName, fileName + '.bak');
        try
          fs := TFileStream.Create(fileName, fmCreate);
        finally
        end;
      end
      else
      begin

      end;
    finally
      fs.Free;
    end;
  end;
end;

procedure TForm1.btn_ExitClick(Sender: TObject);
begin
  Close;
end;

end.

