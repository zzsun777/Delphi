unit BaseForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VGCore_TLB, ActiveX, Utils;

type
  TTBaseForm = class(TForm, IDispatch)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    EventHide: WordBool;
    mWithCMD: WordBool;
    bSettingSaved: WordBool;
    bOptimization: WordBool;
    bEventsEnabled: WordBool;
  protected
    { Private declarations }
    cmdName: string;
    mApp: IVGApplication;
    m_lCookie: Integer;
    {INI 文件设置}
    settingsSection: string;
    settingsIniFilename: string;
    function StartCheck(needDocument: WordBool = True; needShape: WordBool = True): WordBool;
    procedure AddEventListen;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; App: IVGApplication); reintroduce; overload;
  public
    procedure StartEvent(hide: WordBool = False; withCMD: WordBool = True);
    procedure EndEvent;
  public
    function Invoke(dispid: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure QueryDocumentClose(const Doc: IVGDocument; var Cancel: WordBool); dynamic;
    procedure QueryDocumentSave(const Doc: IVGDocument; var Cancel: WordBool); dynamic;
    procedure QueryDocumentPrint(const Doc: IVGDocument; var Cancel: WordBool); dynamic;
    procedure QueryDocumentExport(const Doc: IVGDocument; var Cancel: WordBool); dynamic;
    procedure QueryQuit(var Cancel: WordBool); dynamic;
    procedure DocumentOpen(const Doc: IVGDocument; const FileName: WideString); dynamic;
    procedure DocumentNew(const Doc: IVGDocument; FromTemplate: WordBool; const Template: WideString; IncludeGraphics: WordBool); dynamic;
    procedure DocumentClose(const Doc: IVGDocument); dynamic;
    procedure DocumentBeforeSave(const Doc: IVGDocument; SaveAs: WordBool; const FileName: WideString); dynamic;
    procedure DocumentAfterSave(const Doc: IVGDocument; SaveAs: WordBool; const FileName: WideString); dynamic;
    procedure DocumentBeforePrint(const Doc: IVGDocument); dynamic;
    procedure DocumentAfterPrint(const Doc: IVGDocument); dynamic;
    procedure DocumentBeforeExport(const Doc: IVGDocument; const FileName: WideString; Filter: cdrFilter; SaveBitmap: WordBool); dynamic;
    procedure DocumentAfterExport(const Doc: IVGDocument; const FileName: WideString; Filter: cdrFilter; SaveBitmap: WordBool); dynamic;
    procedure WindowActivate(const Doc: IVGDocument; const Window: IVGWindow); dynamic;
    procedure WindowDeactivate(const Doc: IVGDocument; const Window: IVGWindow); dynamic;
    procedure SelectionChange; dynamic;
    procedure Start; dynamic;
    procedure Quit; dynamic;
    procedure OnPluginCommand(const CommandID: WideString); dynamic;
    procedure OnUpdatePluginCommand(const CommandID: WideString; var Enabled: WordBool; var Checked: cdrCommandCheckState); dynamic;
    procedure OnApplicationEvent(const EventName: WideString; var Parameters: PSafeArray); dynamic;
  end;

var
  TBaseForm: TTBaseForm;

implementation

{$R *.dfm}

constructor TTBaseForm.Create(AOwner: TComponent; App: IVGApplication);
var
  ModuleFileName: array[0..255] of char;
  dllPath: string;
begin
  self.mApp := App;
  m_lCookie := 0;
  AddEventListen;

  GetModuleFileName(GetModuleHandle(PWideChar(GetModuleName(HInstance))), @ModuleFileName[0], SizeOf(ModuleFileName));
  dllPath := ModuleFileName;
  settingsIniFilename := ExtractFilePath(dllPath) + ChangeFileExt(ExtractFileName(GetModuleName(HInstance)), '') + '.ini';
  inherited Create(AOwner);
end;

procedure TTBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if m_lCookie <> 0 then
  begin
    mApp.UnadviseEvents(m_lCookie);
  end;
end;

procedure TTBaseForm.StartEvent(hide: WordBool; withCMD: WordBool);
begin
  EventHide := hide;
  mWithCMD := withCMD;
  if withCMD then
    mApp.ActiveDocument.BeginCommandGroup(cmdName);
  bSettingSaved := False;
  bOptimization := False;
  bEventsEnabled := true;
  //mApp.ActiveDocument.SaveSettings('');
  bSettingSaved := true;
  if EventHide then
  begin
    bOptimization := mApp.Optimization;
    bEventsEnabled := mApp.EventsEnabled;
    mApp.Optimization := true;
    mApp.EventsEnabled := False;
  end;
end;

procedure TTBaseForm.EndEvent;
begin
  if bSettingSaved then
  begin
    if mWithCMD then
      mApp.ActiveDocument.EndCommandGroup;
    //mApp.ActiveDocument.ResetSettings;
  end;
  if EventHide then
  begin
    mApp.Optimization := False;
    mApp.EventsEnabled := bEventsEnabled;
  end;
  mApp.Refresh;
end;

function TTBaseForm.StartCheck(needDocument: WordBool; needShape: WordBool): WordBool;
begin
  if needDocument then
  begin
    if mApp.Documents.Count = 0 then
    begin
      MessageBox(self.Handle, '没有文档被打开！', '错误', MB_OK + MB_ICONSTOP);
      Result := False;
      Exit;
    end;
  end;
  if needShape then
  begin
    if mApp.Documents.Count = 0 then
    begin
      MessageBox(self.Handle, '没有文档被打开！', '错误', MB_OK + MB_ICONSTOP);
      Result := False;
      Exit;
    end;
    if mApp.ActiveDocument.ActivePage.Shapes.Count = 0 then
    begin
      MessageBox(self.Handle, '到少需要一个对象！', '错误', MB_OK + MB_ICONSTOP);
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TTBaseForm.AddEventListen;
begin
  if m_lCookie <> 0 then
    m_lCookie := mApp.AdviseEvents(Self);
end;

function TTBaseForm.Invoke(dispid: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer; ArgErr: Pointer): HRESULT;
var
  Cancel: WordBool;
  DispParams: TDispParams;
begin
  Cancel := False;
  DispParams := TDispParams(Params);
  case dispid of
    DISPID_APP_QUERYDOCUMENTCLOSE:
      begin
        Self.QueryDocumentClose(IVGDocument(DispParams.rgvarg^[1].dispVal), Cancel);
        DispParams.rgvarg^[0].pbool^ := Cancel;
      end;
    DISPID_APP_QUERYDOCUMENTSAVE:
      begin
        Self.QueryDocumentSave(IVGDocument(DispParams.rgvarg^[1].dispVal), Cancel);
        DispParams.rgvarg^[0].pbool^ := Cancel;
      end;
    DISPID_APP_QUERYDOCUMENTPRINT:
      begin
        Self.QueryDocumentPrint(IVGDocument(DispParams.rgvarg^[1].dispVal), Cancel);
        DispParams.rgvarg^[0].pbool^ := Cancel;
      end;
    DISPID_APP_QUERYDOCUMENTEXPORT:
      begin
        Self.QueryDocumentExport(IVGDocument(DispParams.rgvarg^[1].dispVal), Cancel);
        DispParams.rgvarg^[0].pbool^ := Cancel;
      end;
    DISPID_APP_QUERYQUIT:
      begin
        Self.QueryQuit(Cancel);
        DispParams.rgvarg^[0].pbool^ := Cancel;
      end;
    DISPID_APP_DOCUMENTOPEN:
      begin
        Self.DocumentOpen(IVGDocument(DispParams.rgvarg^[1].dispVal), DispParams.rgvarg^[0].pbstrVal^);
      end;
    DISPID_APP_DOCUMENTNEW:
      begin
        Self.DocumentNew(IVGDocument(DispParams.rgvarg^[3].dispVal), DispParams.rgvarg^[2].vbool, DispParams.rgvarg^[1].pbstrVal^, DispParams.rgvarg^[0].vbool);
      end;
    DISPID_APP_DOCUMENTCLOSE:
      begin
        Self.DocumentClose(IVGDocument(DispParams.rgvarg^[0].dispVal));
      end;
    DISPID_APP_DOCUMENTBEFORESAVE:
      begin
        Self.DocumentBeforeSave(IVGDocument(DispParams.rgvarg^[2].dispVal), DispParams.rgvarg^[1].vbool, DispParams.rgvarg^[0].pbstrVal^);
      end;
    DISPID_APP_DOCUMENTAFTERSAVE:
      begin
        self.DocumentAfterSave(IVGDocument(DispParams.rgvarg^[2].dispVal), DispParams.rgvarg^[1].vbool, DispParams.rgvarg^[0].pbstrVal^);
      end;
    DISPID_APP_DOCUMENTBEFOREPRINT:
      begin

      end;
    DISPID_APP_DOCUMENTAFTERPRINT:
      begin

      end;
    DISPID_APP_DOCUMENTBEFOREEXPORT:
      begin

      end;
    DISPID_APP_WINDOWACTIVATE:
      begin

      end;
    DISPID_APP_SELECTIONCHANGE:
      begin
        Self.SelectionChange;
      end;
  end;
  Result := S_OK;
end;

procedure TTBaseForm.QueryDocumentClose(const Doc: IVGDocument; var Cancel: WordBool);
begin

end;

procedure TTBaseForm.QueryDocumentSave(const Doc: IVGDocument; var Cancel: WordBool);
begin

end;

procedure TTBaseForm.QueryDocumentPrint(const Doc: IVGDocument; var Cancel: WordBool);
begin

end;

procedure TTBaseForm.QueryDocumentExport(const Doc: IVGDocument; var Cancel: WordBool);
begin

end;

procedure TTBaseForm.QueryQuit(var Cancel: WordBool);
begin

end;

procedure TTBaseForm.DocumentOpen(const Doc: IVGDocument; const FileName: WideString);
begin

end;

procedure TTBaseForm.DocumentNew(const Doc: IVGDocument; FromTemplate: WordBool; const Template: WideString; IncludeGraphics: WordBool);
begin

end;

procedure TTBaseForm.DocumentClose(const Doc: IVGDocument);
var
  N: Integer;
begin
  N := 1;
  if mApp.VersionMajor > 14 then
  begin
    N := 0;
  end;
  if mApp.Documents.Count = N then
  begin
    Close;
    Exit;
  end;
end;

procedure TTBaseForm.DocumentBeforeSave(const Doc: IVGDocument; SaveAs: WordBool; const FileName: WideString);
begin

end;

procedure TTBaseForm.DocumentAfterSave(const Doc: IVGDocument; SaveAs: WordBool; const FileName: WideString);
begin

end;

procedure TTBaseForm.DocumentBeforePrint(const Doc: IVGDocument);
begin

end;

procedure TTBaseForm.DocumentAfterPrint(const Doc: IVGDocument);
begin

end;

procedure TTBaseForm.DocumentBeforeExport(const Doc: IVGDocument; const FileName: WideString; Filter: TOleEnum; SaveBitmap: WordBool);
begin

end;

procedure TTBaseForm.DocumentAfterExport(const Doc: IVGDocument; const FileName: WideString; Filter: TOleEnum; SaveBitmap: WordBool);
begin

end;

procedure TTBaseForm.WindowActivate(const Doc: IVGDocument; const Window: IVGWindow);
begin

end;

procedure TTBaseForm.WindowDeactivate(const Doc: IVGDocument; const Window: IVGWindow);
begin

end;

procedure TTBaseForm.SelectionChange;
begin

end;

procedure TTBaseForm.Start;
begin

end;

procedure TTBaseForm.Quit;
begin

end;

procedure TTBaseForm.OnPluginCommand(const CommandID: WideString);
begin

end;

procedure TTBaseForm.OnUpdatePluginCommand(const CommandID: WideString; var Enabled: WordBool; var Checked: TOleEnum);
begin

end;

procedure TTBaseForm.OnApplicationEvent(const EventName: WideString; var Parameters: PSafeArray);
begin
end;

end.

