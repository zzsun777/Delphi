unit ApplicationEvent;

interface

uses
  Winapi.ActiveX;

type
  TApplicationEvent = interface(IDispatch)
    function Invoke(dispid: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

implementation

end.

