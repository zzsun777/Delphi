unit Icon2Base64;

interface

uses
  System.Classes, System.SysUtils, Soap.EncdDecd;

const
  WCMN: string = 'WCmnUI_UIItemBmp';

type
  TIcon2Base64 = class
    class function GetBase64(fileName: string; width: Integer = 16; height: Integer = 16): string;
  end;

implementation

class function TIcon2Base64.GetBase64(fileName: string; width, height: Integer): string;
var
  fs: TFileStream;
  b: TBytes;
  bw: TBinaryWriter;
  tp: Integer;
  ss: TStringStream;
begin
  fs := TFileStream.Create(fileName, fmOpenRead);
  SetLength(b, fs.Size);
  fs.ReadBuffer(b, fs.Size);
  fs.Free;
  bw := TBinaryWriter.Create(TMemoryStream.Create);
  bw.Write($FF);
  bw.Write($FF);
  bw.Write(Integer($00100001));
  bw.Write(BytesOf(WCMN));
  bw.Write(Integer(0));
  bw.Write(0);
  bw.Write(0);
  tp := bw.BaseStream.Position;
  bw.Write(b);
  bw.Seek(tp, TSeekOrigin.soBeginning);
  bw.Write(SmallInt(0));
  bw.Write($28);
  bw.Write(0);
  bw.Write(SmallInt(0));
  bw.Write(0);
  bw.Write(4);
  bw.Write(SmallInt(0));
  bw.Write(Integer(width * height));
  bw.Seek(20, TSeekOrigin.soCurrent);
  bw.BaseStream.Position := bw.BaseStream.Size;
  bw.Write(Integer($00F0F0F0));
  bw.Write(Integer($00A0A0A0));
  bw.Write(Integer($00F0F0F0));
  bw.Write(Integer(0));
  ss := TStringStream.Create;
  bw.BaseStream.Seek(0, TSeekOrigin.soBeginning);
  EncodeStream(bw.BaseStream, ss);
  bw.Free;
  Result := Format('%s', [ss.DataString]);
  ss.Free;
end;

end.

