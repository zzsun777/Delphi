unit frmScreen;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.StdCtrls;

type
  TfScreen = class(TTBaseForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    grp1: TGroupBox;
    lbl4: TLabel;
    lbl5: TLabel;
    edt1: TEdit;
    edt2: TEdit;
    edt3: TEdit;
    edt4: TEdit;
    edt5: TEdit;
    edt6: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    px, py: Integer;
  public
    { Public declarations }
  end;

var
  fScreen: TfScreen;

implementation

{$R *.dfm}

procedure TfScreen.FormCreate(Sender: TObject);
var
  dc: HDC;
  h, v: Integer;
begin
  inherited;
  dc := GetDC(0);
  h := GetDeviceCaps(dc, HORZSIZE);
  v := GetDeviceCaps(dc, VERTSIZE);
  edt1.Text := IntToStr(h);
  edt2.Text := IntToStr(v);
  lbl1.Caption := Format('ÆÁÄ»³ß´çÎª:%dºÁÃ×¡Á%dºÁÃ×£¬%fÓ¢´ç', [h, v, sqrt(h * h + v * v)]);
  px := GetDeviceCaps(dc, HORZRES);
  py := GetDeviceCaps(dc, VERTRES);
  lbl2.Caption := Format('ÆÁÄ»·Ö±æÂÊÎª:%d¡Á%d', [px, py]);

  edt5.Text := mApp.ap
end;

end.

