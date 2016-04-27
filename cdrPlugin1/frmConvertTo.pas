unit frmConvertTo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.ComCtrls,
  Vcl.StdCtrls, VGCore_TLB, Utils, System.IOUtils, Winapi.ShlObj;

type
  TfConvertTo = class(TTBaseForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    btn1: TButton;
    rb1: TRadioButton;
    rb2: TRadioButton;
    btn2: TButton;
    ts3: TTabSheet;
    btn_ToCMYK: TButton;
    chk_CFillOutLine: TCheckBox;
    chk_CBMP: TCheckBox;
    btn_OneKey: TButton;
    btn_CompressPic: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn_ToCMYKClick(Sender: TObject);
    procedure btn_OneKeyClick(Sender: TObject);
    procedure btn_CompressPicClick(Sender: TObject);
  private
    { Private declarations }
    procedure ConvertToCurves; overload;
    procedure ConvertToCurves(page: IVGPage); overload;
    procedure ConvertToCurves(ss: IVGShapes); overload;
    procedure FillOutlineToCMYK; overload;
    procedure FillOutlineToCMYK(page: IVGPage); overload;
    procedure FillOutlineToCMYK(ss: IVGShapes); overload;
    procedure CompressPic; overload;
    procedure CompressPic(page: IVGPage); overload;
    procedure CompressPic(ss: IVGShapes); overload;
  public
    { Public declarations }
  end;

var
  fConvertTo: TfConvertTo;

implementation

{$R *.dfm}

procedure TfConvertTo.ConvertToCurves;
var
  I: Integer;
begin
  self.cmdName := '批量转曲';
  StartEvent(True);
  if rb1.Checked then
  begin
    ConvertToCurves(mApp.ActivePage);
  end
  else
  begin
    for I := 1 to mApp.ActiveDocument.Pages.Count do
    begin
      ConvertToCurves(mApp.ActiveDocument.Pages[I]);
    end;
  end;
  EndEvent;
  MessageBox(Handle, '所有文字已转曲！', '提示', MB_OK + MB_ICONINFORMATION);
end;

procedure TfConvertTo.ConvertToCurves(page: IVGPage);
begin
  page.Activate;
  ConvertToCurves(page.Shapes);
end;

procedure TfConvertTo.ConvertToCurves(ss: IVGShapes);
var
  ss1: IVGShapeRange;
  I: Integer;
begin
  ss1 := ss.FindShapes('', cdrTextShape, True, '');
  for I := 1 to ss1.Count do
  begin
    ss1[I].ConvertToCurves;
  end;
  ss1 := ss.FindShapes('', cdrNoShape, True, '@com.PowerClip <> null');
  for I := 1 to ss1.Count do
  begin
    ConvertToCurves(ss1[I].PowerClip.Shapes);
  end;
end;

procedure TfConvertTo.btn1Click(Sender: TObject);
begin
  inherited;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        ConvertToCurves;
      except
        on e: Exception do
        begin
          debugUtils.ShowMessage(e.Message);
        end;
      end;
    end).Start;
end;

procedure TfConvertTo.btn_CompressPicClick(Sender: TObject);
begin
  inherited;
  CompressPic;
end;

procedure TfConvertTo.btn_OneKeyClick(Sender: TObject);
var
  s: string;
  sa: IVGStructSaveAsOptions;
begin
  inherited;
  s := InputBox('请输入', '请输入文件名：', mApp.ActiveDocument.Name);
  if not s.ToLower.EndsWith('.cdr') then
  begin
    s := s + '.cdr';
  end;
  rb2.Checked := True;
  ConvertToCurves;
  FillOutlineToCMYK;
  sa := mApp.CreateStructSaveAsOptions;
  sa.Version := cdrVersion14;
  mApp.ActiveDocument.SaveAs(GetFolderPath(CSIDL_DESKTOP) + '\' + s, sa);
  MessageBox(Handle, '文件已存至桌面！', '提示', MB_OK + MB_ICONINFORMATION);
end;

procedure TfConvertTo.btn_ToCMYKClick(Sender: TObject);
begin
  inherited;
  if chk_CFillOutLine.Checked then
  begin
    FillOutlineToCMYK;
  end;
end;

procedure TfConvertTo.FillOutlineToCMYK;
var
  I: Integer;
begin
  Self.cmdName := '转换填充轮廓为CMYK';
  StartEvent(True);
  if rb1.Checked then
  begin
    FillOutlineToCMYK(mApp.ActivePage);
  end
  else
  begin
    for I := 1 to mApp.ActiveDocument.Pages.Count do
    begin
      FillOutlineToCMYK(mApp.ActiveDocument.Pages[I]);
    end;
  end;
  EndEvent;
  MessageBox(Handle, '所有填充轮廓已转换为CMYK！', '提示', MB_OK + MB_ICONINFORMATION);
end;

procedure TfConvertTo.FillOutlineToCMYK(page: IVGPage);
begin
  page.Activate;
  FillOutlineToCMYK(page.Shapes);
end;

procedure TfConvertTo.FillOutlineToCMYK(ss: IVGShapes);
var
  ss1: IVGShapeRange;
  s: IVGShape;
  I, I1: Integer;
  ff: IVGFountainFill;
begin
  for I := 1 to ss.Count do
  begin
    s := ss[I];
    if s.Fill.type_ = cdrUniformFill then
    begin
      if s.Fill.UniformColor.type_ = cdrColorRGB then
      begin
        s.Fill.UniformColor.ConvertToCMYK;
      end;
    end
    else if s.Fill.type_ = cdrFountainFill then
    begin
      ff := s.Fill.Fountain;
      for I1 := 0 to ff.Colors.Count - 1 do
      begin
        if ff.Colors[I1].Color.type_ = cdrColorRGB then
        begin
          ff.Colors[I1].Color.ConvertToCMYK;
        end;
      end;
    end;
    if s.type_ = cdrOutline then
    begin
      if s.Outline.Color.type_ = cdrColorRGB then
      begin
        s.Outline.Color.ConvertToCMYK;
      end;
    end;
  end;
  ss1 := ss.FindShapes('', cdrNoShape, True, '@com.PowerClip <> null');
  for I := 1 to ss1.Count do
  begin
    FillOutlineToCMYK(ss1[I].PowerClip.Shapes);
  end;
end;

procedure TfConvertTo.CompressPic;
var
  I: Integer;
begin
  self.cmdName := '批量压缩图片';
  StartEvent(True);
  if rb1.Checked then
  begin
    CompressPic(mApp.ActivePage);
  end
  else
  begin
    for I := 1 to mApp.ActiveDocument.Pages.Count do
    begin
      CompressPic(mApp.ActiveDocument.Pages[I]);
    end;
  end;
  EndEvent;
  MessageBox(Handle, '所有图片已压缩！', '提示', MB_OK + MB_ICONINFORMATION);
end;

procedure TfConvertTo.CompressPic(page: IVGPage);
begin
  page.Activate;
  CompressPic(page.Shapes);
end;

procedure TfConvertTo.CompressPic(ss: IVGShapes);
var
  ss1: IVGShapeRange;
  s: IVGShape;
  I: Integer;
  b: IVGBitmap;
begin
  ss1 := ss.FindShapes('', cdrBitmapShape, True, '');
  for I := 1 to ss1.Count do
  begin
    s := ss1[I];
    b := s.Bitmap;
    if (b.ResolutionX <= 300) and (b.ResolutionY <= 300) then
    begin
      Continue;
    end;
    s.ConvertToBitmapEx(b.Mode, False, True, 300, cdrNormalAntiAliasing, True, True, 100);
  end;
  ss1 := ss.FindShapes('', cdrNoShape, True, '@com.PowerClip <> null');
  for I := 1 to ss1.Count do
  begin
    s := ss1[I];
    s.PowerClip.EnterEditMode;
    CompressPic(mApp.ActiveLayer.Shapes);
    s.PowerClip.LeaveEditMode;
  end;
end;

end.

