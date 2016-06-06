unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VGCore_TLB, BaseForm, Vcl.StdCtrls,
  frmToJPG, frmConvertTo, frmScreen, frmCropMark, frmAlign, frmAllCommand,
  frmFontRecognition, frmTest;

type
  TfMain = class(TTBaseForm)
    btn_ToJPG: TButton;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    procedure btn_ToJPGClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
  private
    f: TTBaseForm;
  public
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btn1Click(Sender: TObject);
begin
  inherited;
  if not Assigned(fConvertTo) then
    fConvertTo := TfConvertTo.Create(Self, FApp);
  fConvertTo.Show;
end;

procedure TfMain.btn2Click(Sender: TObject);
begin
  inherited;
  if not Assigned(fScreen) then
    fScreen := TfScreen.Create(Self, FApp);
  fScreen.Show;
end;

procedure TfMain.btn3Click(Sender: TObject);
begin
  inherited;
  if not Assigned(fAlign) then
    fAlign := TfAlign.Create(Self, FApp);
  fAlign.Show;
end;

procedure TfMain.btn4Click(Sender: TObject);
begin
  inherited;
  if not Assigned(fAllCommand) then
    fAllCommand := TfAllCommand.Create(self, FApp);
  fAllCommand.PageAdaptation;
end;

procedure TfMain.btn5Click(Sender: TObject);
begin
  inherited;
  if not Assigned(fFontRecognition) then
    fFontRecognition := TfFontRecognition.Create(self, FApp);
  fFontRecognition.Show;
end;

procedure TfMain.btn_ToJPGClick(Sender: TObject);
begin
  inherited;
  if not Assigned(fToJPG) then
    fToJPG := TfToJPG.Create(Self, FApp);
  fToJPG.Show;
end;

end.

