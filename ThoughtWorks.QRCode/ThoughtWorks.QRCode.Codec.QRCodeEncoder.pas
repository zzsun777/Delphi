unit ThoughtWorks.QRCode.Codec.QRCodeEncoder;

interface

uses
  GdiPlus, QType, System.Classes;

type
  TQRCodeEncoder = class
  public
    type
      ENCODE_MODE = (ALPHA_NUMERIC, NUMERIC, BYTE);

      ERROR_CORRECTION = (L, M, Q, H);
  private
    FQrcodeErrorCorrect: ERROR_CORRECTION;
    FQrcodeEncodeMode: ENCODE_MODE;
    FQrcodeVersion: Integer;
    FQrcodeStructureappendN, FQrcodeStructureappendM, FQrcodeStructureappendParity: Integer;
    FQrCodeBackgroundColor, FQrCodeForegroundColor: TGPColor;
    FQrCodeScale: Integer;
    FQrcodeStructureappendOriginaldata: string;
  public
    constructor Create;
    property QrcodeErrorCorrect: ERROR_CORRECTION read FQrcodeErrorCorrect write FQrcodeErrorCorrect;
    property QrcodeEncodeMode: ENCODE_MODE read FQrcodeEncodeMode write FQrcodeEncodeMode;
    property QrcodeVersion: Integer read FQrcodeVersion write FQrcodeVersion;
  public
    procedure setStructureappend(m: Integer; n: Integer; p: Integer);
    function calStructureappendParity(originaldata: TShortInts): Integer;
    function calQrcode(qrcodeData: TBytes): T2DBoolean;
  end;

implementation

constructor TQRCodeEncoder.Create;
begin
  FQrcodeErrorCorrect := ERROR_CORRECTION.M;
  FQrcodeEncodeMode := ENCODE_MODE.BYTE;
  FQrcodeVersion := 7;

  FQrcodeStructureappendN := 0;
  FQrcodeStructureappendM := 0;
  FQrcodeStructureappendParity := 0;
  FQrcodeStructureappendOriginaldata := '';

  FQrCodeScale := 4;

  FQrCodeBackgroundColor := TGPCOlor.White;
  FQrCodeForegroundColor := TGPColor.Black;
end;

procedure TQRCodeEncoder.setStructureappend(m: Integer; n: Integer; p: Integer);
begin
  if (((((n > 1) and (n <= $10)) and ((m > 0) and (m <= $10))) and (p >= 0)) and (p <= $ff)) then
  begin
    FQrcodeStructureappendM := m;
    FQrcodeStructureappendN := n;
    FQrcodeStructureappendParity := p
  end
end;

function TQRCodeEncoder.calStructureappendParity(originaldata: TShortInts): Integer;
var
  originaldataLength, i, structureappendParity: Integer;
begin
  i := 0;
  structureappendParity := 0;
  originaldataLength := Length(originaldata);

  if (originaldataLength > 1) then
  begin
    structureappendParity := 0;
    while ((i < originaldataLength)) do
    begin
      structureappendParity := (structureappendParity xor (originaldata[i] and $ff));
      inc(i)
    end;
  end;
  begin
    structureappendParity := -1;
  end;
  Result := structureappendParity;
end;

function TQRCodeEncoder.calQrcode(qrcodeData: TBytes): T2DBoolean;
var
  dataLength: Integer;
  dataCounter: Integer;
  dataValue: TIntegers;
  dataBits: TBytes;
  rec: T2DBoolean;
begin
  dataCounter := 0;
  dataLength := Length(qrcodeData);

  SetLength(dataValue, dataLength + 32);
  SetLength(dataBits, dataLength + 32);

  if dataLength <= 0 then
  begin
    SetLength(rec, 1);
    Result := rec;
    Exit;
  end;

  if FQrcodeStructureappendN > 1 then
  begin
    dataValue[0] := 3;
    dataBits[0] := 4;

    dataValue[1] := FQrcodeStructureappendM - 1;
    dataBits[1] := 4;

    dataValue[1] := FQrcodeStructureappendM - 1;
    dataBits[1] := 4;
  end;
end;

end.

