unit frmConvertTo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.ComCtrls,
  Vcl.StdCtrls, VGCore_TLB, Utils;

type
  TfConvertTo = class(TTBaseForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    btn1: TButton;
    rb1: TRadioButton;
    rb2: TRadioButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ConvertToCurves; overload;
    procedure ConvertToCurves(page: IVGPage); overload;
    procedure ConvertToCurves(ss: IVGShapes); overload;
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
  ConvertToCurves(page.Shapes);
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

end.

