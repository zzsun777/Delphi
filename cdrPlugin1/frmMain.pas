unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VGCore_TLB, BaseForm, Vcl.StdCtrls,
  frmToJPG, frmConvertTo;

type
  TfMain = class(TTBaseForm)
    btn_ToJPG: TButton;
    btn1: TButton;
    procedure btn_ToJPGClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; App: IVGApplication); reintroduce; overload;
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

procedure TfMain.btn_ToJPGClick(Sender: TObject);
var
  frm_ToJPG: TfToJPG;
begin
  inherited;
  frm_ToJPG := TfToJPG.Create(self, mapp);
  frm_ToJPG.Show;
end;

constructor TfMain.Create(AOwner: TComponent; App: IVGApplication);
begin
  self.mApp := App;
  inherited Create(AOwner);
end;

end.

