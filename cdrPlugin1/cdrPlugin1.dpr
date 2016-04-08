library cdrPlugin1;




{$R *.dres}

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  VGCore_TLB in 'X8\VGCore_TLB.pas',
  frmMain in 'frmMain.pas' {fMain},
  BaseForm in 'BaseForm.pas' {TBaseForm},
  frmToJPG in 'frmToJPG.pas' {fToJPG},
  frmTest in 'frmTest.pas' {fTest},
  Utils in 'Utils.pas',
  frmConvertTo in 'frmConvertTo.pas' {fConvertTo},
  frmScreen in 'frmScreen.pas' {fScreen},
  frmCropMark in 'frmCropMark.pas' {fCropMark},
  CnConsts in 'cnvcl\CnConsts.pas',
  CnSpin in 'cnvcl\CnSpin.pas',
  Generics.Collections,
  frmOnekeyPS in 'frmOnekeyPS.pas' {fOnekeyPS};

{$R *.res}

var
  g_hResource: HINST = 0;

type
  TisnPlugin = class(TObject, IVGAppPlugin, IDispatch, IUnknown)
    const
      CommandBarName: WideString = 'tisn201600401';
      CommandID_ConvertTo: WideString = 'cdrplugin1_转换';
      CommandID_ToJPG: WideString = 'cdrplugin1_导出图片';
      CommandID_CropMark: WideString = 'cdrplugin1_裁切标记';
  private
    mApp: IVGApplication;
    m_lCookie: longint;
    m_ulRefCount: ULONG;
    myCommandBar: CommandBar;
    cmdList: TDictionary<WideString, WideString>;
    procedure OnAppStart; safecall;
    procedure AddPluginCommands;
    procedure RemovePluginCommands;
    procedure AddButton(ID, Icon: WideString; guid: WideString = '');
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
  cmdList := TDictionary<WideString, WideString>.Create;
  cmdList.Add(CommandID_ToJPG, '导出图片');
  cmdList.Add(CommandID_ConvertTo, '转换');
  cmdList.Add(CommandID_CropMark, '裁切标记');
end;

procedure TisnPlugin.OnAppStart;
var
  i: Integer;
begin
  AddPluginCommands;
  if mApp.VersionMajor >= 18 then
  begin
    try
      myCommandBar := mApp.CommandBars.Item[CommandBarName];
    except
      myCommandBar := self.mApp.CommandBars.Add(CommandBarName, cuiBarFloating, False);
      myCommandBar.Visible := true;
    end;
    for I := 1 to myCommandBar.Controls.Count do
    begin
      myCommandBar.Controls.Remove(1);
    end;
    AddButton(CommandID_ToJPG, '', '57e469be-c42a-41d4-9892-c7ac0b00cd79');
    AddButton(CommandID_ConvertTo, '', 'b9bd86de-975c-4b2a-a3c3-2601dfb08bd0');
    AddButton(CommandID_CropMark, '', '7013f31a-dc2e-41d8-bb73-f48ce435f3de');
  end
  else
  begin
    try
      myCommandBar := mApp.CommandBars.Item[CommandBarName];
    except
      myCommandBar := self.mApp.CommandBars.Add(CommandBarName, cuiBarFloating, False);
      myCommandBar.Visible := true;
      try
        AddButton(CommandID_ToJPG, 'ToJPG');
        AddButton(CommandID_ConvertTo, 'ConvertTo');
        AddButton(CommandID_CropMark, 'CropMark');
      except
        on o: Exception do
        begin
          MessageBox(0, PWideChar(o.Message), '0123', 0);
        end;

      end;
    end;
  end;
end;

procedure TisnPlugin.AddPluginCommands;
var
  pair: TPair<WideString, WideString>;
begin
  for pair in cmdList do
  begin
    mApp.AddPluginCommand(pair.Key, pair.Value, pair.Value);
  end;
end;

procedure TisnPlugin.RemovePluginCommands;
var
  pair: TPair<WideString, WideString>;
begin
  for pair in cmdList do
  begin
    mApp.RemovePluginCommand(pair.Key);
  end;
end;

procedure TisnPlugin.AddButton(ID: WideString; Icon: WideString; guid: WideString);
var
  btn: ICUIControl;
  bmp: TBitmap;
  fn: string;
begin
  btn := myCommandBar.Controls.AddCustomButton(cdrCmdCategoryPlugins, ID, 0, False);
  if mApp.VersionMajor >= 18 then
  begin
    btn.SetIcon2('guid://' + guid);
  end
  else
  begin
    bmp := TBitmap.Create;
    bmp.LoadFromResourceName(HInstance, 'Bitmap_' + Icon);
    fn := mApp.CorelScriptTools.GetTempFolder + '\tisntmp.bmp';
    bmp.SaveToFile(fn);
    bmp.Destroy;
    {X8不支持此方法}
    btn.SetCustomIcon(fn);
    DeleteFile(fn);
  end;
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
    RemovePluginCommands;

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
  strCMD: WideString;
  f: TTBaseForm;
  DispParams: TDispParams;
begin
  f := nil;
  DispParams := TDispParams(Params);
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
        if Variant(DispParams.cArgs = 1) then
        begin
          strCMD := DispParams.rgvarg^[0].bstrVal;
          if strCMD = CommandID_ToJPG then
          begin
            f := TfMain.Create(nil, mApp);
          end
          else if strCMD = CommandID_ConvertTo then
          begin
            f := TfConvertTo.Create(nil, mApp);
          end
          else if strCMD = CommandID_CropMark then
          begin
            f := TfCropMark.Create(nil, mApp);
          end;
          if f <> nil then
          begin
            f.Show;
          end;
        end;
      end;
    $0015:
      begin // DISPID_APP_ONPLUGINCMDSTATE
        if DispParams.cArgs = 3 then
        begin
          strCMD := DispParams.rgvarg^[2].bstrVal;
          if cmdList.ContainsKey(strCMD) then
          begin
            DispParams.rgvarg^[1].pbool^ := mApp.Documents.Count > 0;
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

