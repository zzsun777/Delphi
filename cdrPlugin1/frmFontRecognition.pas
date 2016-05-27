unit frmFontRecognition;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.StdCtrls;

type
  TfFontRecognition = class(TTBaseForm)
    cbb_FontList: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fFontRecognition: TfFontRecognition;

implementation

{$R *.dfm}

procedure TfFontRecognition.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  cbb_FontList.Clear;
  for I := 1 to mApp.FontList.Count do
  begin
    //cbb_FontList.Items.Add(mApp.FontList.Item[I]);
  end;
end;

end.

