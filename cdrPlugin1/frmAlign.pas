unit frmAlign;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.StdCtrls,
  Vcl.Buttons, System.ImageList, Vcl.ImgList, SpeedButtonEx, Vcl.ExtCtrls,
  VGCore_TLB;

type
  TfAlign = class(TTBaseForm)
    il1: TImageList;
    spdbtnxALeft: TSpeedButtonEx;
    spdbtnxARight: TSpeedButtonEx;
    spdbtnxAHCenter: TSpeedButtonEx;
    lbl1: TLabel;
    bvl1: TBevel;
    spdbtnxCCenter: TSpeedButtonEx;
    spdbtnxABottom: TSpeedButtonEx;
    spdbtnxATop: TSpeedButtonEx;
    chkSpace: TCheckBox;
    edtSpace: TEdit;
    spdbtnxSLeft: TSpeedButtonEx;
    spdbtnxSHCenter: TSpeedButtonEx;
    spdbtnxSHSpace: TSpeedButtonEx;
    spdbtnxSRight: TSpeedButtonEx;
    spdbtnxSCSpace: TSpeedButtonEx;
    spdbtnxSBottom: TSpeedButtonEx;
    spdbtnxSCCenter: TSpeedButtonEx;
    spdbtnxSTop: TSpeedButtonEx;
    lbl2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure spdbtnxALeftClick(Sender: TObject);
    procedure spdbtnxARightClick(Sender: TObject);
    procedure spdbtnxAHCenterClick(Sender: TObject);
    procedure spdbtnxATopClick(Sender: TObject);
    procedure spdbtnxABottomClick(Sender: TObject);
    procedure spdbtnxCCenterClick(Sender: TObject);
    procedure chkSpaceClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fAlign: TfAlign;

implementation

{$R *.dfm}

procedure TfAlign.chkSpaceClick(Sender: TObject);
begin
  inherited;
  edtSpace.Enabled := chkSpace.Checked;
end;

procedure TfAlign.FormCreate(Sender: TObject);
begin
  inherited;
  il1.GetBitmap(0, spdbtnxALeft.Glyph);
  il1.GetBitmap(1, spdbtnxARight.Glyph);
  il1.GetBitmap(2, spdbtnxAHCenter.Glyph);
  il1.GetBitmap(3, spdbtnxATop.Glyph);
  il1.GetBitmap(4, spdbtnxABottom.Glyph);
  il1.GetBitmap(5, spdbtnxCCenter.Glyph);
  il1.GetBitmap(6, spdbtnxSLeft.Glyph);
  il1.GetBitmap(7, spdbtnxSHCenter.Glyph);
  il1.GetBitmap(8, spdbtnxSRight.Glyph);
  il1.GetBitmap(9, spdbtnxSHSpace.Glyph);
  il1.GetBitmap(10, spdbtnxSTop.Glyph);
  il1.GetBitmap(11, spdbtnxSCCenter.Glyph);
  il1.GetBitmap(12, spdbtnxSBottom.Glyph);
  il1.GetBitmap(13, spdbtnxSCSpace.Glyph);
  chkSpaceClick(nil);
end;

procedure TfAlign.spdbtnxABottomClick(Sender: TObject);
var
  sr: IVGShapeRange;
begin
  inherited;
  if FApp.ActiveSelection.Shapes.Count < 2 then
  begin
    Exit;
  end;
  Self.cmdName := '底对齐';
  StartEvent;
  sr := FApp.ActiveSelectionRange;
  sr.AlignAndDistribute(cdrAlignDistributeHNone, cdrAlignDistributeVAlignBottom, cdrAlignShapesToLastSelected, cdrDistributeToSelection);
  EndEvent;
end;

procedure TfAlign.spdbtnxAHCenterClick(Sender: TObject);
var
  sr: IVGShapeRange;
begin
  inherited;
  if FApp.ActiveSelection.Shapes.Count < 2 then
  begin
    Exit;
  end;
  Self.cmdName := '水平居中对齐';
  StartEvent;
  sr := FApp.ActiveSelectionRange;
  sr.AlignAndDistribute(cdrAlignDistributeHAlignCenter, cdrAlignDistributeVNone, cdrAlignShapesToLastSelected, cdrDistributeToSelection);
  EndEvent;
end;

procedure TfAlign.spdbtnxALeftClick(Sender: TObject);
var
  sr: IVGShapeRange;
begin
  inherited;
  if FApp.ActiveSelection.Shapes.Count < 2 then
  begin
    Exit;
  end;
  Self.cmdName := '左对齐';
  StartEvent;
  sr := FApp.ActiveSelectionRange;
  sr.AlignAndDistribute(cdrAlignDistributeHAlignLeft, cdrAlignDistributeVNone, cdrAlignShapesToLastSelected, cdrDistributeToSelection);
  EndEvent;
end;

procedure TfAlign.spdbtnxARightClick(Sender: TObject);
var
  sr: IVGShapeRange;
begin
  inherited;
  if FApp.ActiveSelection.Shapes.Count < 2 then
  begin
    Exit;
  end;
  Self.cmdName := '右对齐';
  StartEvent;
  sr := FApp.ActiveSelectionRange;
  sr.AlignAndDistribute(cdrAlignDistributeHAlignRight, cdrAlignDistributeVNone, cdrAlignShapesToLastSelected, cdrDistributeToSelection);
  EndEvent;
end;

procedure TfAlign.spdbtnxATopClick(Sender: TObject);
var
  sr: IVGShapeRange;
begin
  inherited;
  if FApp.ActiveSelection.Shapes.Count < 2 then
  begin
    Exit;
  end;
  Self.cmdName := '顶对齐';
  StartEvent;
  sr := FApp.ActiveSelectionRange;
  sr.AlignAndDistribute(cdrAlignDistributeHNone, cdrAlignDistributeVAlignTop, cdrAlignShapesToLastSelected, cdrDistributeToSelection);
  EndEvent;
end;

procedure TfAlign.spdbtnxCCenterClick(Sender: TObject);
var
  sr: IVGShapeRange;
begin
  inherited;
  if FApp.ActiveSelection.Shapes.Count < 2 then
  begin
    Exit;
  end;
  Self.cmdName := '垂直居中对齐';
  StartEvent;
  sr := FApp.ActiveSelectionRange;
  sr.AlignAndDistribute(cdrAlignDistributeHNone, cdrAlignDistributeVAlignCenter, cdrAlignShapesToLastSelected, cdrDistributeToSelection);
  EndEvent;
end;

end.

