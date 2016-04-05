unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VGCore_TLB, BaseForm, Vcl.StdCtrls,
  frmToJPG, frmConvertTo, frmScreen, frmCropMark;

type
  TfMain = class(TTBaseForm)
    btn_ToJPG: TButton;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    procedure btn_ToJPGClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
  private
    f: TTBaseForm;
  public
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btn1Click(Sender: TObject);
var
  frm_ConvertTo: TfConvertTo;
begin
  inherited;
  frm_ConvertTo := TfConvertTo.Create(Self, mApp);
  frm_ConvertTo.Show;
end;

procedure TfMain.btn2Click(Sender: TObject);
begin
  inherited;
  f := TfScreen.Create(self, mApp);
  f.Show;
end;

procedure TfMain.btn3Click(Sender: TObject);
begin
  inherited;
  f := TfCropMark.Create(Self, mApp);
  f.Show;
end;

procedure TfMain.btn_ToJPGClick(Sender: TObject);
var
  frm_ToJPG: TfToJPG;
begin
  inherited;
  frm_ToJPG := TfToJPG.Create(self, mapp);
  frm_ToJPG.Show;
end;

end.

