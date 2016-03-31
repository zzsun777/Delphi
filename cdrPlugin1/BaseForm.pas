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

  GetModuleFileName(GetModuleHandle(PWideChar(GetModuleName(HInstance))), @ModuleFileName[0], SizeOf(ModuleFileName));
  dllPath := ModuleFileName;
  settingsIniFilename := ExtractFilePath(dllPath) + 'cdrPlugin1.ini';
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
      MessageBox(self.Handle, '没有文档被打开！', '错误', 0);
      Result := False;
      Exit;
    end;
  end
  else if needShape then
  begin
    if mApp.Documents.Count = 0 then
    begin
      MessageBox(self.Handle, '没有文档被打开！', '错误', 0);
      Result := False;
      Exit;
    end;
    if mApp.ActiveDocument.ActivePage.Shapes.Count = 0 then
    begin
      MessageBox(self.Handle, '到少需要一个对象！', '错误', 0);
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TTBaseForm.AddEventListen;
begin
  m_lCookie := mApp.AdviseEvents(self);
  mApp.EventsEnabled := True;
end;

end.

