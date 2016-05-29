unit frmQrcode;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, QRGraphics, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, DelphiZXIngQRCode, QR_Win1251, QR_URL, System.IOUtils,
  VGCore_TLB, SVGImage, CaptureImageTool, ScanManager, BarcodeFormat, ReadResult,
  QRCodeReader, pCore2D, pBarcode2D, pQRCode;

type
  TfQrcode = class(TTBaseForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    ts3: TTabSheet;
    btn_Cap: TButton;
    btn_Gen: TButton;
    mmo_Self: TMemo;
    pbPreview: TPaintBox;
    cbbDrawingMode: TComboBox;
    cbbErrorCorrectionLevel: TComboBox;
    lbl1: TLabel;
    edt_CardName: TEdit;
    lbl2: TLabel;
    edt_CardNumber: TEdit;
    lbl3: TLabel;
    lbl4: TLabel;
    edt_CardQQ: TEdit;
    edt_CardPosition: TEdit;
    lbl5: TLabel;
    lbl6: TLabel;
    edt_CardWeb: TEdit;
    edt_CardEmail: TEdit;
    lbl7: TLabel;
    edt_CardCompany: TEdit;
    lbl8: TLabel;
    edt_CardAdd: TEdit;
    lbl9: TLabel;
    mmo_RealText: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure pbPreviewPaint(Sender: TObject);
    procedure mmo_SelfChange(Sender: TObject);
    procedure cbbErrorCorrectionLevelChange(Sender: TObject);
    procedure btn_GenClick(Sender: TObject);
    procedure cbbDrawingModeChange(Sender: TObject);
    procedure edt_CardNameChange(Sender: TObject);
    procedure btn_CapClick(Sender: TObject);
  private
    mQRCode: TDelphiZXingQRCode;
    procedure ReMakeQR;
    procedure refreshIDCard;
  public
    { Public declarations }
  end;

var
  fQrcode: TfQrcode;

implementation

{$R *.dfm}

procedure TfQrcode.btn_CapClick(Sender: TObject);
var
  cap: TTCaptureImageTool;
  img: TBitmap;
  stream: TStream;
  scan: TScanManager;
  rResult: TReadResult;
  decoder: TQRCodeReader;
begin
  inherited;
  cap := TTCaptureImageTool.Create(Self);
  if cap.ShowModal = mrOk then
  begin
    img := cap.Image;
    stream := TMemoryStream.Create;
    img.SaveToStream(stream);
    decoder := TQRCodeReader.Create;
    stream.Seek(0, TSeekOrigin.soBeginning);
    rResult := decoder.decode(stream);
    if rResult.Text <> '' then
    begin
      with mQRCode do
      try
        BeginUpdate;
        Data := rResult.Text;
    //Encoding := cmbEncoding.ItemIndex;
        ErrorCorrectionOrdinal := TErrorCorrectionOrdinal(cbbErrorCorrectionLevel.ItemIndex);
    //QuietZone := StrToIntDef(edtQuietZone.Text, 4);
        EndUpdate(True);
    //lblQRMetrics.Caption := IntToStr(Columns) + 'x' + IntToStr(Rows) + ' (' + IntToStr(Columns - QuietZone * 2) + 'x' + IntToStr(Rows - QuietZone * 2) + ')';
      finally
        pbPreview.Repaint;
      end;
    end
    else
    begin

    end;
  end;
end;

procedure TfQrcode.btn_GenClick(Sender: TObject);
var
  svg: TSVGImage;
  tp: string;
begin
  inherited;
  svg := MakeSvgImagefile(0, mQRCode, clBlack, clWhite, TQRDrawingMode(cbbDrawingMode.ItemIndex div 2), 0);
  tp := mApp.CorelScriptTools.GetTempFolder + '\tiiiii.eps';
  svg.SaveToFile(tp);
  mApp.ActiveLayer.Import(tp, cdrAutoSense, nil);
  //mApp.ActiveShape.Rotate(90);
  //DeleteFile(tp);
end;

procedure TfQrcode.cbbDrawingModeChange(Sender: TObject);
begin
  inherited;
  ReMakeQR;
end;

procedure TfQrcode.cbbErrorCorrectionLevelChange(Sender: TObject);
begin
  inherited;
  ReMakeQR;
end;

procedure TfQrcode.edt_CardNameChange(Sender: TObject);
begin
  inherited;
  refreshIDCard;
end;

procedure TfQrcode.FormCreate(Sender: TObject);
begin
  inherited;
  mQRCode := TDelphiZXingQRCode.Create;
  mQRCode.RegisterEncoder(ENCODING_WIN1251, TWin1251Encoder);
  mQRCode.RegisterEncoder(ENCODING_URL, TURLEncoder);

end;

procedure TfQrcode.mmo_SelfChange(Sender: TObject);
begin
  inherited;
  ReMakeQR;
end;

procedure TfQrcode.pbPreviewPaint(Sender: TObject);
begin
  inherited;
  with pbPreview.Canvas do
  begin
    //Pen.Color := clrbxForeground.Selected;
    //Brush.Color := clrbxBackground.Selected;


  end;
  DrawQR(pbPreview.Canvas, pbPreview.ClientRect, mQRCode, 0, TQRDrawingMode(cbbDrawingMode.ItemIndex div 2), Boolean(1 - cbbDrawingMode.ItemIndex mod 2));
end;

procedure TfQrcode.ReMakeQR;
begin
  with mQRCode do
  try
    BeginUpdate;
    Data := mmo_Self.Lines.Text;
    //Encoding := cmbEncoding.ItemIndex;
    ErrorCorrectionOrdinal := TErrorCorrectionOrdinal(cbbErrorCorrectionLevel.ItemIndex);
    //QuietZone := StrToIntDef(edtQuietZone.Text, 4);
    EndUpdate(True);
    //lblQRMetrics.Caption := IntToStr(Columns) + 'x' + IntToStr(Rows) + ' (' + IntToStr(Columns - QuietZone * 2) + 'x' + IntToStr(Rows - QuietZone * 2) + ')';
  finally
    pbPreview.Repaint;
  end;
end;

procedure TfQrcode.refreshIDCard;
var
  qText: TStrings;
  d: Boolean;
begin
  qText := TStringList.Create;
  qText.Append('BEGIN:VCARD');
  d := False;
  if edt_CardName.Text <> '' then
  begin
    qText.Append(Format('FN:%s', [edt_CardName.Text]));
    d := True;
  end;
  if edt_CardPosition.Text <> '' then
  begin
    qText.Append(Format('TITLE:%s', [edt_CardPosition.Text]));
    d := True;
  end;
  if edt_CardWeb.Text <> '' then
  begin
    qText.Append(Format('URL:%s', [edt_CardWeb.Text]));
    d := True;
  end;
  if edt_CardNumber.Text <> '' then
  begin
    qText.Append(Format('TEL;CELL:%s', [edt_CardWeb.Text]));
    d := True;
  end;
  if edt_CardQQ.Text <> '' then
  begin
    qText.Append(Format('X-QQ:%s', [edt_CardWeb.Text]));
    d := True;
  end;
  if edt_CardEmail.Text <> '' then
  begin
    qText.Append(Format('EMAIL:%s', [edt_CardWeb.Text]));
    d := True;
  end;
  if edt_CardCompany.Text <> '' then
  begin
    qText.Append(Format('ORG:%s', [edt_CardWeb.Text]));
    d := True;
  end;
  if edt_CardAdd.Text <> '' then
  begin
    qText.Append(Format('ADD;WORK:%s', [edt_CardWeb.Text]));
    d := True;
  end;
  qText.Append('END:VCARD');
  if not d then
  begin
    qText.Clear;
  end;
  mmo_RealText.Lines := qText;
  with mQRCode do
  try
    BeginUpdate;
    Data := qText.Text;
    //Encoding := cmbEncoding.ItemIndex;
    ErrorCorrectionOrdinal := TErrorCorrectionOrdinal(cbbErrorCorrectionLevel.ItemIndex);
    //QuietZone := StrToIntDef(edtQuietZone.Text, 4);
    EndUpdate(True);
    //lblQRMetrics.Caption := IntToStr(Columns) + 'x' + IntToStr(Rows) + ' (' + IntToStr(Columns - QuietZone * 2) + 'x' + IntToStr(Rows - QuietZone * 2) + ')';
  finally
    pbPreview.Repaint;
  end;
end;

end.

