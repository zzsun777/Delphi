unit Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Zip,
  System.IniFiles, Winapi.ShlObj, System.IOUtils, Generics.Collections;

var
  OldWow64RedirectionValue: LongBool = True;
  /// <summary>
  /// 判断系统是否为64位
  /// </summary>
  /// <returns></returns>

function IsWin64: Boolean;

function DisableWowRedirection: Boolean;

function RevertWowRedirection: Boolean;

implementation

function IsWin64: Boolean;
var
  Kernel32Handle: THandle;
  IsWow64Process: function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  GetNativeSystemInfo: procedure(var lpSystemInfo: TSystemInfo); stdcall;
  isWoW64: Bool;
  SystemInfo: TSystemInfo;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
begin
  Kernel32Handle := GetModuleHandle('KERNEL32.DLL');
  if Kernel32Handle = 0 then
    Kernel32Handle := LoadLibrary('KERNEL32.DLL');
  if Kernel32Handle <> 0 then
  begin
    IsWOW64Process := GetProcAddress(Kernel32Handle, 'IsWow64Process');
    GetNativeSystemInfo := GetProcAddress(Kernel32Handle, 'GetNativeSystemInfo');
    if Assigned(IsWow64Process) then
    begin
      IsWow64Process(GetCurrentProcess, isWoW64);
      Result := isWoW64 and Assigned(GetNativeSystemInfo);
      if Result then
      begin
        GetNativeSystemInfo(SystemInfo);
        Result := (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) or (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64);
      end;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

function DisableWowRedirection: Boolean;
type
  TWow64DisableWow64FsRedirection = function(var b: LongBool): LongBool; stdcall;
var
  hHandle: THandle;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
begin
  Result := true;
  try
    hHandle := GetModuleHandle('kernel32.dll');
    @Wow64DisableWow64FsRedirection := GetProcAddress(hHandle, 'Wow64DisableWow64FsRedirection');
    if ((hHandle <> 0) and (@Wow64DisableWow64FsRedirection <> nil)) then
      Wow64DisableWow64FsRedirection(OldWow64RedirectionValue);
  except
    Result := False;
  end;
end;

function RevertWowRedirection: Boolean;
type
  TWow64RevertWow64FsRedirection = function(var b: LongBool): LongBool; stdcall;
var
  hHandle: THandle;
  Wow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection;
begin
  Result := true;
  try
    hHandle := GetModuleHandle('kernel32.dll');
    @Wow64RevertWow64FsRedirection := GetProcAddress(hHandle, 'Wow64RevertWow64FsRedirection');
    if ((hHandle <> 0) and (@Wow64RevertWow64FsRedirection <> nil)) then
      Wow64RevertWow64FsRedirection(OldWow64RedirectionValue);
  except
    Result := False;
  end;
end;

end.

