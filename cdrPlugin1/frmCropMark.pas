unit frmCropMark;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.StdCtrls,
  Vcl.ComCtrls, CnSpin, VGCore_TLB, Vcl.Samples.Spin, Winapi.ActiveX;

type
  TfCropMark = class(TTBaseForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    grp1: TGroupBox;
    lbl1: TLabel;
    nudLen: TCnSpinEdit;
    nudOffset: TCnSpinEdit;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    btn_OK: TButton;
    grp2: TGroupBox;
    chk_Chu: TCheckBox;
    nudCu: TSpinEdit;
    lbl5: TLabel;
    lbl6: TLabel;
    chkZxSP: TCheckBox;
    lbl7: TLabel;
    chkZxCZ: TCheckBox;
    chkJdCZ: TCheckBox;
    chkJdSP: TCheckBox;
    lbl8: TLabel;
    procedure btn_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DoDrawCropMarks;
    function DrawLine(x1, y1, x2, y2: Double; colr: IVGColor = nil; style: IVGOutlineStyle = nil): IVGShape;
  public
    { Public declarations }
  end;

var
  fCropMark: TfCropMark;

implementation

{$R *.dfm}

procedure TfCropMark.btn_OKClick(Sender: TObject);
begin
  inherited;
  TThread.CreateAnonymousThread(
    procedure
    begin
      DoDrawCropMarks;
    end).Start;
end;

procedure TfCropMark.DoDrawCropMarks;
var
  x, y, cx, cy: Double;
  sx, sy, dx, dy: Double;
  Off, l, Cu: Double;
  N: LONG;
  sr, AllShapes: IVGShapeRange;
  s, s1, tmps: IVGShape;
  clr: IVGColor;
  otl: IVGOutlineStyle;
  v: Variant;
const
  dash: array[0..2] of Integer = (2, 2, 5);
begin
  self.StartEvent(True);
  sr := mApp.CreateShapeRange;
  AllShapes := mApp.CreateShapeRange;
  with mApp.ActiveDocument do
  begin
    Unit_ := cdrMillimeter;
    PreserveSelection := False;
    Selection.GetBoundingBox(x, y, cx, cy, False);
  end;
  Off := nudOffset.Value;
  l := nudLen.Value;
  sr.Add(DrawLine(x - off - l, y, x - off, y));
  sr.Add(DrawLine(X - Off - l, y + cy, X - Off, y + cy));
  sr.Add(DrawLine(X + cx + Off, y, X + cx + Off + l, y));
  sr.Add(DrawLine(X + cx + Off, y + cy, X + cx + Off + l, y + cy));
  sr.Add(DrawLine(X, y - Off - l, X, y - Off));
  sr.Add(DrawLine(X + cx, y - Off - l, X + cx, y - Off));
  sr.Add(DrawLine(X, y + Off + l + cy, X, y + Off + cy));
  sr.Add(DrawLine(X + cx, y + Off + l + cy, X + cx, y + Off + cy));
  AllShapes.Add(sr.Group);

  if chk_Chu.Checked then
  begin
    Cu := nudCu.Value;
    sr.RemoveAll;
    sr.Add(DrawLine(X - Off - l, y + Cu, X - Off, y + Cu));
    sr.Add(DrawLine(X - Off - l, y + cy - Cu, X - Off, y + cy - Cu));
    sr.Add(DrawLine(X + cx + Off, y + Cu, X + cx + Off + l, y + Cu));
    sr.Add(DrawLine(X + cx + Off, y + cy - Cu, X + cx + Off + l, y + cy - Cu));
    sr.Add(DrawLine(X + Cu, y - Off - l, X + Cu, y - Off));
    sr.Add(DrawLine(X + cx - Cu, y - Off - l, X + cx - Cu, y - Off));
    sr.Add(DrawLine(X + Cu, y + Off + l + cy, X + Cu, y + Off + cy));
    sr.Add(DrawLine(X + cx - Cu, y + Off + l + cy, X + cx - Cu, y + Off + cy));
    AllShapes.Add(sr.Group);
  end;
  clr := mApp.CreateCMYKColor(0, 100, 100, 0);
  if chkZxSP.Checked then
  begin
    sr.RemoveAll;

    v := VarArrayCreate([2, 2], varInteger);
    otl := mApp.CreateOutlineStyle(3, PSafeArray(TVarData(v).VArray));
    otl.DashCount := 5;

    sr.Add(DrawLine(X - Off - l, y + cy / 2, X, y + cy / 2, clr, otl));
    sr.Add(DrawLine(X + cx, y + cy / 2, X + cx + Off + l, y + cy / 2, clr, otl));

    AllShapes.Add(sr.Group);
  end;

  if chkZxCZ.Checked then
  begin
    sr.RemoveAll();
    sr.Add(DrawLine(X + cx / 2, y + cy, X + cx / 2, y + cy + Off + l, clr));
    sr.Add(DrawLine(X + cx / 2, y, X + cx / 2, y - Off - l, clr));
    AllShapes.Add(sr.Group);
  end;

  AllShapes.Group;
  self.EndEvent;
end;

function TfCropMark.DrawLine(x1: Double; y1: Double; x2: Double; y2: Double; colr: IVGColor = nil; style: IVGOutlineStyle = nil): IVGShape;
var
  s: IVGShape;
begin
  s := mApp.ActiveLayer.CreateLineSegment(x1, y1, x2, y2);
  s.Outline.Width := 0.1;
  if colr <> nil then
  begin
    s.Outline.Color := colr;
  end
  else
  begin
    s.Outline.Color.CMYKAssign(0, 0, 0, 100);
  end;
  if style <> nil then
  begin
    s.Outline.Style := style;
  end;
  Result := s;
end;

procedure TfCropMark.FormCreate(Sender: TObject);
begin
  inherited;
  if not StartCheck then
  begin
    Free;
    exit;
  end;
  self.cmdName := '²ÃÇÐ±ê¼Ç';
end;

end.

