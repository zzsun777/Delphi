unit MyUtils;

interface

uses
  System.SysUtils, System.Variants, System.Classes;

type
  HexUtility = class
  public
    class function Replace(var all: TBytes; s, t: TBytes): Integer;
  end;

implementation

function HexUtility.Replace(var all: TBytes; s: TBytes; t: TBytes): Integer;
var
  replace_count, I, J, K: Integer;
  temp: TList;
  catch_s: WordBool;
begin
  temp := TList.Create;
  if Length(s) > Length(all) then
  begin
    Result := 0;
    Exit;
  end;
  replace_count := 0;
  for I := 0 to (Length(all) - Length(s)) + 1 do
  begin
    catch_s := True;
    for J := 0 to Length(s) do
    begin
      if all[i + j] <> s[j] then
      begin
        catch_s := False;
        break;
      end;
    end;
    if catch_s then
    begin
      Inc(replace_count);
      for K := 0 to Length(t) do
      begin
        temp.Add(t[K]);
      end;
      i := i + Length(s) - 1;
    end
    else
    begin
      temp.Add(all[I]);
    end;
    if I = Length(all) - Length(s) then
    begin
      if not catch_s then
      begin
        temp.Add(all[Length(all) - 2]);
        temp.Add(all[Length(all) - 1]);
      end;
    end;
  end;
  all := temp.It
end;

end.

