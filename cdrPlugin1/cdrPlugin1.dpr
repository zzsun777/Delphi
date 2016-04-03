library cdrPlugin1;


{$R *.dres}

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  VGCore_TLB in '.\X8\VGCore_TLB.pas',
  frmMain in 'frmMain.pas' {fMain},
  BaseForm in 'BaseForm.pas' {TBaseForm},
  frmToJPG in 'frmToJPG.pas' {fToJPG},
  frmTest in 'frmTest.pas' {fTest},
  Utils in 'Utils.pas',
  ApplicationEvent in 'ApplicationEvent.pas',
  frmConvertTo in 'frmConvertTo.pas' {fConvertTo},
  frmScreen in 'frmScreen.pas' {fScreen};

{$R *.res}

var
  g_hResource: HINST = 0;

type
  TisnPlugin = class(TObject, IVGAppPlugin, IDispatch, IUnknown)
    const
      CommandID_ConvertTo: string = 'tisn2016_转换';
      CommandID_ToJPG: string = 'tisn2016_导出图片';
  private
    mApp: IVGApplication;
    m_lCookie: longint;
    m_ulRefCount: ULONG;
    m_bEnabled: Boolean;
    myCommandBar: CommandBar;
    procedure OnAppStart; safecall;
    procedure AddButton(ID, Icon: WideString);
  public
    constructor Create;
  public
    //IVGAppPlugin
    procedure OnLoad(const Application: IVGApplication); safecall;
    procedure StartSession; safecall;
    procedure StopSession; safecall;
    procedure OnUnload; safecall;
  public
    //IDispatch
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(dispid: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

constructor TisnPlugin.Create;
begin
  m_ulRefCount := 0;
  //MessageBox(0, PWideChar(IntTostr(g_hResource)), 'Create', 0);
end;

procedure TisnPlugin.OnAppStart;
var
  i: Integer;
begin
  mApp.AddPluginCommand(CommandID_ToJPG, '导出图片', '导出图片');
  mApp.AddPluginCommand(CommandID_ConvertTo, '转换', '转换');
  if mApp.VersionMajor < 19 then
  begin
    try
      myCommandBar := mApp.CommandBars.Item['tisn99'];
    except
      myCommandBar := self.mApp.CommandBars.Add('tisn99', cuiBarFloating, True);
      myCommandBar.Visible := true;
      AddButton(CommandID_ToJPG, 'ToJPG');
      AddButton(CommandID_ConvertTo, 'ConvertTo');
    end;
  end
  else
  begin

  end;
end;

procedure TisnPlugin.AddButton(ID: WideString; Icon: WideString);
var
  btn: ICUIControl;
  bmp: TBitmap;
  icn: TIcon;
  fn: string;
begin
  btn := myCommandBar.Controls.AddCustomButton(cdrCmdCategoryPlugins, ID, 0, False);
  btn.Visible := True;
  bmp := TBitmap.Create;
  bmp.LoadFromResourceName(HInstance, 'Bitmap_' + Icon);
  fn := mApp.CorelScriptTools.GetTempFolder + '\tisntmp.bmp';
  bmp.SaveToFile(fn);
  bmp.Destroy;
  btn.SetCustomIcon(fn);
  DeleteFile(fn);
end;

procedure TisnPlugin.OnLoad(const Application: IVGApplication);
begin
  self.mApp := Application;
  if self.mApp <> nil then
  begin
    self.mApp._AddRef;
  end;

end;

procedure TisnPlugin.StartSession;
begin
  try
    self.m_lCookie := self.mApp.AdviseEvents(self);
  except
    on E: Exception do
      MessageBox(0, PWideChar(E.Message + e.StackTrace), 'StartSession', 0);
  end;
end;

procedure TisnPlugin.StopSession;
begin
  try
    self.mApp.UnadviseEvents(self.m_lCookie);
    self.mApp.RemovePluginCommand(CommandID_ToJPG);
    self.mApp.RemovePluginCommand(CommandID_ConvertTo);

    //X4中不会自动释放，导致关闭CorelDraw程序后进程不会退出，所以在这手动释放一下
    self.mApp._Release;
    self.Destroy;
  except
    on E: Exception do
      MessageBox(0, PWideChar(E.Message + e.StackTrace), 'StopSession', 0);
  end;
end;

procedure TisnPlugin.OnUnload;
begin

  if self.mApp <> nil then
  begin
    self.mApp._Release;
    self.Destroy;
  end;
end;

function TisnPlugin.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TisnPlugin.GetTypeInfo(Index: Integer; LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TisnPlugin.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TisnPlugin.Invoke(dispid: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
var
  nHandle: Integer;
  hAppWnd: HWND;
  strCMD: WideString;
  f: TTBaseForm;
begin
  //nHandle := mApp.AppWindow.Handle;
  //hAppWnd := HWND(nHandle);
  case dispid of
    $0011:
      begin // DISPID_APP_SELCHANGE

      end;
    $0012:
      begin // DISPID_APP_START
        self.OnAppStart;
      end;
    $0014:
      begin // DISPID_APP_ONPLUGINCMD
      {单击按钮事件}
        if Variant(TDispParams(Params).cArgs = 1) then
        begin
          strCMD := TDispParams(Params).rgvarg^[0].bstrVal;
          if strCMD = CommandID_ToJPG then
          begin
            f := TfToJPG.Create(nil, mApp);
            f.Show;
            {frm_Main := TfMain.Create(nil, mApp);
            frm_Main.Show;}
          end
          else if strCMD = CommandID_ConvertTo then
          begin
            f := TfConvertTo.Create(nil, mApp);
            f.Show;
          end;
        end;
      end;
    $0015:
      begin // DISPID_APP_ONPLUGINCMDSTATE
      {图标是否有效}
        if TDispParams(Params).cArgs = 3 then
        begin
          strCMD := TDispParams(Params).rgvarg^[2].bstrVal;
          if (strCMD = CommandID_ToJPG) or (strCMD = CommandID_ConvertTo) then
          begin
            TDispParams(Params).rgvarg^[1].pbool^ := mApp.Documents.Count > 0;
          end;
        end;
      end;
  end;
  Result := S_OK;
end;

function TisnPlugin.QueryInterface(const IID: TGUID; out Obj): Hresult;
var
  hr: HResult;
begin
  hr := S_OK;
  //Inc(m_ulRefCount);
  if (IID = IID_IUnknown) then
  begin
    IUnknown(Obj) := self;
  end
  else if (IID = IID_IDispatch) then
  begin
    IDispatch(Obj) := self;
  end
  else if (IID = IID_IVGAppPlugin) then
  begin
    IVGAppPlugin(Obj) := Self;
  end
  else
  begin
    //Dec(m_ulRefCount);
    hr := E_NOINTERFACE;
  end;
  Result := hr;
end;

//此处很重要
function TisnPlugin._AddRef;
begin
  inc(self.m_ulRefCount);
  //OutputDebugString(PWideChar(Format('_AddRef:%d', [m_ulRefCount])));
  Result := self.m_ulRefCount;
end;

//此处很重要
function TisnPlugin._Release;
begin
  dec(self.m_ulRefCount);
  //OutputDebugString(PWideChar(Format('_Release:%d', [m_ulRefCount])));
  if (self.m_ulRefCount = 0) then
  begin
    Destroy;
  end;
  Result := self.m_ulRefCount;
end;

function AttachPlugin(var ppIPlugin): ULONG; stdcall;
begin
  IVGAppPlugin(ppIPlugin) := TisnPlugin.Create;
  Result := $100;
end;

function DllEnterPoint(Reason: Integer): Boolean;
begin
  case Reason of
    DLL_PROCESS_ATTACH:
      begin
        g_hResource := HInstance;
      end;
  end;
  Result := True;
end;

exports
  AttachPlugin;

begin
  DllProc := @DllEnterPoint;
  DllEnterPoint(DLL_PROCESS_ATTACH);
end.

