unit CUtils;

interface

uses
  ArrayEx, System.SysUtils, System.IniFiles, System.Classes, Xml.XMLIntf, Xml.XMLDoc,
  Xml.Win.msxmldom, Xml.xmldom, Vcl.Forms, Winapi.Windows, Icon2Base64, System.IOUtils,
  System.Zip, Vcl.Graphics;

type
  cdrVersion = (cdrVersion14, cdrVersion15, cdrVersion16, cdrVersion17, cdrVersion18);

type
  SCommandButtonType = (SCommandButtonTypeNormal, SCommandButtonTypeSegmentation, SCommandButtonTypeSub);

type
  SCommandButton = class
  public
    Type_: SCommandButtonType;
    Guid: string;
    DynamicCommand: string;
    Caption: string;
    IconPath: string;
    IconGuid: string;
    ToolTip: string;
    Parent : SCommandButton;
  public
    constructor Create(sGuid: string = '53175317-5317-5317-5317-900000000001'; sCmd: string = 'tisnplugin_'; sIconPath: string = ''; sCaption: string = '新按钮'; sToolTip: string = '新按钮'; sIconGuid: string = '');
  end;

type
  SegmentationCommandButton = class(SCommandButton)
    constructor Create;
  end;

type
  SubCommandBar = class(SCommandButton)
  public
    Buttons: TArrayEx<SCommandButton>;
    FlyoutBarRef: string;
    constructor Create(sGuid: string = '53175317-5317-5317-5317-900000000001'; sCmd: string = 'tisnplugin_'; sIconPath: string = ''; sFlyoutBarRef: string = ''; sCaption: string = '新子菜单'; sToolTip: string = '新按钮'; sIconGuid: string = ''); overload;
    procedure AddCommandButton(button: SCommandButton);
  end;

type
  SCommandBar = class
  private
    mOwner: TComponent;
    mXmlDoc: IXMLDocument;
    mMainNode: IXMLNode;
  public
    Guid: string;
    Name: string;
    Caption: string;
    Buttons: TArrayEx<SCommandButton>;
  public
    constructor Create(AOwner: TComponent; fileName: string); overload;
    destructor Destroy; overload;
    procedure AddCommandButton(button: SCommandButton);
    procedure ToX4(fileName: string);
    procedure ToX7(fileName: string);
    procedure ToX8(fileName: string);
    procedure SaveToFile(fileName: string);
  end;

implementation

constructor SCommandButton.Create(sGuid: string; sCmd: string; sIconPath: string; sCaption: string; sToolTip: string; sIconGuid: string);
begin
  Self.Type_ := SCommandButtonType.SCommandButtonTypeNormal;
  Self.Guid := sGuid;
  Self.DynamicCommand := sCmd;
  Self.IconPath := sIconPath;
  Self.Caption := sCaption;
  Self.ToolTip := sToolTip;
  Self.IconGuid := sIconGuid;
end;

constructor SegmentationCommandButton.Create;
begin
  Self.Type_ := SCommandButtonType.SCommandButtonTypeSegmentation;
  Self.Guid := '266435b4-6e53-460f-9fa7-f45be187d400';
end;

constructor SubCommandBar.Create(sGuid: string; sCmd: string; sIconPath: string; sFlyoutBarRef: string; sCaption: string; sToolTip: string; sIconGuid: string);
begin
  Self.Type_ := SCommandButtonType.SCommandButtonTypeSub;
  Self.Guid := sGuid;
  Self.DynamicCommand := sCmd;
  Self.IconPath := sIconPath;
  Self.FlyoutBarRef := sFlyoutBarRef;
  Self.Caption := sCaption;
  Self.ToolTip := sToolTip;
  Self.IconGuid := sIconGuid;
end;

procedure SubCommandBar.AddCommandButton(button: SCommandButton);
begin
  Buttons.Insert(Buttons.Size, button);
end;

constructor SCommandBar.Create(AOwner: TComponent; fileName: string);
var
  tmpNode: IXMLNode;
  t: SCommandButtonType;
  aGuid, aCmd, aIconPath, aFlyoutBarRef, aUserCaption, aToolTip, aIconGuid: string;
  cmdButton: SCommandButton;
  I: Integer;
begin
  mOwner := AOwner;
  mXmlDoc := TXMLDocument.Create(AOwner);
  mXmlDoc.Active := True;
  mXmlDoc.Options := mXmlDoc.Options + [doNodeAutoIndent];
  if not FileExists(fileName) then
  begin
    Buttons := [];
    Guid := '53175317-5317-5317-5317-100005000001';
    Name := '新建工具栏T1';
    Caption := '新建工具栏';

    mXmlDoc.Encoding := 'UTF-8';
    mXmlDoc.Version := '1.0';
    mMainNode := mXmlDoc.AddChild('CommandBar');
    mMainNode.Attributes['Name'] := Name;
    mMainNode.Attributes['Caption'] := Caption;
    mMainNode.Attributes['Guid'] := Guid;
    Exit;
  end;
  mXmlDoc.LoadFromFile(fileName);
  mMainNode := mXmlDoc.DocumentElement;
  Name := mMainNode.Attributes['Name'];
  Caption := mMainNode.Attributes['Caption'];
  Guid := mMainNode.Attributes['Guid'];
  for I := 0 to mMainNode.ChildNodes.Count - 1 do
  begin
    tmpNode := mMainNode.ChildNodes[I];
    t := tmpNode.Attributes['Type'];
    case t of
      SCommandButtonTypeNormal:
        begin
          aGuid := tmpNode.Attributes['Guid'];
          aCmd := tmpNode.Attributes['Command'];
          aIconPath := tmpNode.Attributes['IconPath'];
          if tmpNode.HasAttribute('IconGuid') then
          begin
            aIconGuid := tmpNode.Attributes['IconGuid'];
          end;
          aUserCaption := tmpNode.Attributes['Caption'];
          if tmpNode.HasAttribute('ToolTip') then
          begin
            aToolTip := tmpNode.Attributes['ToolTip'];
          end;
          cmdButton := SCommandButton.Create(aGuid, aCmd, aIconPath, aUserCaption, aToolTip, aIconGuid);
        end;
      SCommandButtonTypeSegmentation:
        begin
          cmdButton := SegmentationCommandButton.Create;
        end;
      SCommandButtonTypeSub:
        begin
          aGuid := tmpNode.Attributes['Guid'];
          aIconPath := tmpNode.Attributes['IconPath'];
          if tmpNode.HasAttribute('IconGuid') then
          begin
            aIconGuid := tmpNode.Attributes['IconGuid'];
          end;
          if tmpNode.HasAttribute('FlyoutBarRef') then
          begin
            aFlyoutBarRef := tmpNode.Attributes['FlyoutBarRef'];
          end;
          if tmpNode.HasAttribute('ToolTip') then
          begin
            aToolTip := tmpNode.Attributes['ToolTip'];
          end;
          cmdButton := SubCommandBar.Create(aGuid, aCmd, aIconPath, aFlyoutBarRef, aUserCaption, aToolTip, aIconGuid);
        end;
    end;
    Buttons.Insert(Buttons.Len, cmdButton);
  end;
end;

destructor SCommandBar.Destroy;
begin
  mMainNode := nil;
  mXmlDoc := nil;
  inherited;
end;

procedure SCommandBar.AddCommandButton(button: SCommandButton);
begin
  Buttons.Insert(Buttons.Size, button);
end;

procedure SCommandBar.ToX4(fileName: string);
var
  rs: TResourceStream;
  tmpXmlDoc: IXMLDocument;
  rootNode, tmpNode1, tmpNode2, tmpNode3: IXMLNode;
  cmdButton, cmdButton1: SCommandButton;
  subBar: SubCommandBar;
begin
  rs := TResourceStream.Create(HInstance, 'X4Empty', RT_RCDATA);
  rs.SaveToFile(TPath.GetTempPath + '\ttttmp.xml');
  rs.Destroy;
  tmpXmlDoc := TXMLDocument.Create(mOwner);
  tmpXmlDoc.Active := True;
  tmpXmlDoc.Options := tmpXmlDoc.Options + [doNodeAutoIndent];
  tmpXmlDoc.LoadFromFile(TPath.GetTempPath + '\ttttmp.xml');
  DeleteFile(PWideChar(TPath.GetTempPath + '\ttttmp.xml'));
  rootNode := tmpXmlDoc.DocumentElement;

  tmpNode1 := rootNode.ChildNodes.FindNode('export:data', '');
  tmpNode1 := tmpNode1.ChildNodes.FindNode('uiConfig', '');
  tmpNode1 := tmpNode1.ChildNodes.FindNode('commandBars', '');
  tmpNode2 := tmpNode1.AddChild('commandBarData');
  tmpNode2.Attributes['guid'] := Guid;
  tmpNode2.Attributes['type'] := 'toolbar';
  tmpNode2.Attributes['nonLocalizableName'] := Name;
  tmpNode2.Attributes['userCaption'] := Self.Caption;
  tmpNode2.Attributes['userCreate'] := 'true';
  tmpNode2 := tmpNode2.AddChild('container');
  for cmdButton in Buttons do
  begin
    tmpNode3 := tmpNode2.AddChild('item');
    tmpNode3.Attributes['guidRef'] := cmdButton.Guid;
  end;

  for cmdButton in Buttons do
  begin
    if cmdButton.Type_ = SCommandButtonType.SCommandButtonTypeSub then
    begin
      subBar := SubCommandBar(cmdButton);
      tmpNode2 := tmpNode1.AddChild('commandBarData');
      tmpNode2.Attributes['guid'] := subBar.FlyoutBarRef;
      tmpNode2.Attributes['type'] := 'toolbar';
      tmpNode2.Attributes['userCaption'] := subBar.Caption;
      tmpNode2.Attributes['userToopTip'] := subBar.ToolTip;
      tmpNode2.Attributes['userCreate'] := 'true';
      tmpNode2 := tmpNode2.AddChild('container');
      for cmdButton1 in subBar.Buttons do
      begin
        tmpNode3 := tmpNode2.AddChild('item');
        tmpNode3.Attributes['guidRef'] := cmdButton1.Guid;
      end;
    end;
  end;

  tmpNode1 := rootNode.ChildNodes.FindNode('export:data', '');
  tmpNode1 := tmpNode1.ChildNodes.FindNode('uiConfig', '');
  tmpNode1 := tmpNode1.ChildNodes.FindNode('items', '');
  for cmdButton in Buttons do
  begin
    tmpNode2 := tmpNode1.AddChild('itemData');
    case cmdButton.Type_ of
      SCommandButtonTypeNormal:
        begin
          tmpNode2.Attributes['guid'] := cmdButton.Guid;
          tmpNode2.Attributes['dynamicCommand'] := cmdButton.DynamicCommand;
          tmpNode2.Attributes['dynamicCategory'] := 'ab489730-8791-45d2-a825-b78bbe0d6a5d';
          tmpNode2.Attributes['userCaption'] := cmdButton.Caption;
          tmpNode2.Attributes['userToopTip'] := cmdButton.ToolTip;
          if FileExists(cmdButton.IconPath) then
          begin
            tmpNode3 := tmpNode2.AddChild('userSmallBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(cmdButton.IconPath);

            tmpNode3 := tmpNode2.AddChild('userLargeBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(cmdButton.IconPath, 24, 24);
          end;
        end;
      SCommandButtonTypeSegmentation:
        begin
          tmpNode2.Attributes['guidRef'] := '266435b4-6e53-460f-9fa7-f45be187d400';
        end;
      SCommandButtonTypeSub:
        begin
          subBar := SubCommandBar(cmdButton);
          tmpNode2.Attributes['guid'] := subBar.Guid;
          tmpNode2.Attributes['flyoutBarRef'] := subBar.FlyoutBarRef;
          tmpNode2.Attributes['captionRef'] := 'bc6d3e54-d8cd-449a-aa61-cd442d6617eb';
          tmpNode2.Attributes['tooltipRef'] := 'bc6d3e54-d8cd-449a-aa61-cd442d6617eb';
          tmpNode2.Attributes['userCreated'] := 'true';
          tmpNode2.Attributes['userCaption'] := subBar.Caption;
          tmpNode2.Attributes['userToopTip'] := subBar.ToolTip;
          if FileExists(subBar.IconPath) then
          begin
            tmpNode3 := tmpNode2.AddChild('userSmallBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(subBar.IconPath);

            tmpNode3 := tmpNode2.AddChild('userLargeBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(subBar.IconPath, 24, 24);
          end;
        end;
    end;
  end;
  tmpNode1 := rootNode.AddChild('xsl:template');
  tmpNode1.Attributes['match'] := 'node()[count(*)=''0'']|@*';
  tmpNode1 := tmpNode1.AddChild('xsl:copy-of');
  tmpNode1.Attributes['select'] := '.';

  tmpNode1 := rootNode.AddChild('xsl:template');
  tmpNode1.Attributes['match'] := 'node()';
  tmpNode1 := tmpNode1.AddChild('xsl:copy').AddChild('xsl:apply-templates');
  tmpNode1.Attributes['select'] := 'node()|@*';

  tmpNode1 := rootNode.AddChild('xsl:template');
  tmpNode1.Attributes['match'] := 'uiConfig';
  tmpNode1 := tmpNode1.AddChild('xsl:copy');
  tmpNode2 := tmpNode1.AddChild('xsl:apply-templates');
  tmpNode2.Attributes['select'] := 'node()|@*';
  for cmdButton in Buttons do
  begin
    case cmdButton.Type_ of
      SCommandButtonTypeNormal, SCommandButtonTypeSegmentation:
        begin
          tmpNode2 := tmpNode1.AddChild('xsl:if');
          tmpNode2.Attributes['test'] := 'not(./items)';
          tmpNode2 := tmpNode2.AddChild('xsl:copy-of');
          tmpNode2.Attributes['select'] := 'document('''')/*/export:data/uiConfig/items';
        end;
      SCommandButtonTypeSub:
        begin
          tmpNode2 := tmpNode1.AddChild('xsl:if');
          tmpNode2.Attributes['test'] := 'not(./commandBars)';
          tmpNode2 := tmpNode2.AddChild('xsl:copy-of');
          tmpNode2.Attributes['select'] := 'document('''')/*/export:data/uiConfig/commandBars';
        end;
    end;
  end;
  tmpNode2 := tmpNode1.AddChild('xsl:if');
  tmpNode2.Attributes['test'] := 'not(./commandBars)';
  tmpNode2 := tmpNode2.AddChild('xsl:copy-of');
  tmpNode2.Attributes['select'] := 'document('''')/*/export:data/uiConfig/commandBars';

  tmpNode1 := rootNode.AddChild('xsl:template');
  tmpNode1.Attributes['match'] := 'uiConfig/items';
  tmpNode1 := tmpNode1.AddChild('xsl:copy');
  tmpNode2 := tmpNode1.AddChild('xsl:apply-templates');
  tmpNode2.Attributes['select'] := 'node()|@*';
  for cmdButton in Buttons do
  begin
    tmpNode2 := tmpNode1.AddChild('xsl:if');
    tmpNode2.Attributes['test'] := Format('not(./itemData[@guid=''%s''])', [cmdButton.Guid]);
    tmpNode2 := tmpNode2.AddChild('xsl:copy-of');
    tmpNode2.Attributes['select'] := Format('document('''')/*/export:data/uiConfig/items/itemData[@guid=''%s'']', [cmdButton.Guid]);
  end;

  for cmdButton in Buttons do
  begin
    tmpNode1 := rootNode.AddChild('xsl:template');
    tmpNode1.Attributes['match'] := Format('uiConfig/items/itemData[@guid=''%s'']', [cmdButton.Guid]);
    tmpNode1 := tmpNode1.AddChild('xsl:copy-of');
    tmpNode1.Attributes['select'] := Format('document('''')/*/export:data/uiConfig/items/itemData[@guid=''%s'']', [cmdButton.Guid]);
  end;

  tmpNode1 := rootNode.AddChild('xsl:template');
  tmpNode1.Attributes['match'] := 'uiConfig/commandBars';
  tmpNode1 := tmpNode1.AddChild('xsl:copy');
  tmpNode2 := tmpNode1.AddChild('xsl:apply-templates');
  tmpNode2.Attributes['select'] := 'node()|@*';
  for cmdButton in Buttons do
  begin
    if cmdButton.Type_ = SCommandButtonType.SCommandButtonTypeSub then
    begin
      tmpNode2 := tmpNode1.AddChild('xsl:if');
      tmpNode2.Attributes['test'] := Format('not(./commandBarData[@guid=''%s''])', [SubCommandBar(cmdButton).FlyoutBarRef]);
      tmpNode2 := tmpNode2.AddChild('xsl:copy-of');
      tmpNode2.Attributes['select'] := Format('document('''')/*/export:data/uiConfig/commandBars/commandBarData[@guid=''%s'']', [SubCommandBar(cmdButton).FlyoutBarRef]);
    end;
  end;
  tmpNode2 := tmpNode1.AddChild('xsl:if');
  tmpNode2.Attributes['test'] := Format('not(./commandBarData[@guid=''%s''])', [Guid]);
  tmpNode2 := tmpNode2.AddChild('xsl:copy-of');
  tmpNode2.Attributes['select'] := Format('document('''')/*/export:data/uiConfig/commandBars/commandBarData[@guid=''%s'']', [Guid]);

  for cmdButton in Buttons do
  begin
    if cmdButton.Type_ = SCommandButtonType.SCommandButtonTypeSub then
    begin
      tmpNode1 := rootNode.AddChild('xsl:template');
      tmpNode1.Attributes['match'] := Format('uiConfig/commandBars/commandBarData[@guid=''%s'']', [SubCommandBar(cmdButton).FlyoutBarRef]);
      tmpNode1 := tmpNode1.AddChild('xsl:copy-of');
      tmpNode1.Attributes['select'] := Format('document('''')/*/export:data/uiConfig/commandBars/commandBarData[@guid=''%s'']', [SubCommandBar(cmdButton).FlyoutBarRef]);
    end;
  end;
  tmpNode1 := rootNode.AddChild('xsl:template');
  tmpNode1.Attributes['match'] := Format('uiConfig/commandBars/commandBarData[@guid=''%s'']', [Guid]);
  tmpNode1 := tmpNode1.AddChild('xsl:copy-of');
  tmpNode1.Attributes['select'] := Format('document('''')/*/export:data/uiConfig/commandBars/commandBarData[@guid=''%s'']', [Guid]);

  tmpXmlDoc.SaveToFile(fileName);
end;

procedure SCommandBar.ToX7(fileName: string);
var
  rs: TResourceStream;
  tmpXmlDoc: IXMLDocument;
  rootNode, tmpNode1, tmpNode2, tmpNode3: IXMLNode;
  cmdButton, cmdButton1: SCommandButton;
  subBar: SubCommandBar;
  zipFile: TZipFile;
begin
  rs := TResourceStream.Create(HInstance, 'X8Empty1', RT_RCDATA);
  rs.SaveToFile(TPath.GetTempPath + '\ttttmp.xml');
  rs.Destroy;
  tmpXmlDoc := TXMLDocument.Create(mOwner);
  tmpXmlDoc.Active := True;
  tmpXmlDoc.Options := tmpXmlDoc.Options + [doNodeAutoIndent];
  tmpXmlDoc.LoadFromFile(TPath.GetTempPath + '\ttttmp.xml');
  DeleteFile(PWideChar(TPath.GetTempPath + '\ttttmp.xml'));
  rootNode := tmpXmlDoc.DocumentElement;

  tmpNode1 := rootNode.ChildNodes.FindNode('commandBars', '');
  tmpNode2 := tmpNode1.AddChild('commandBarData');
  tmpNode2.Attributes['guid'] := Guid;
  tmpNode2.Attributes['type'] := 'toolbar';
  tmpNode2.Attributes['nonLocalizableName'] := Name;
  tmpNode2.Attributes['userCaption'] := Self.Caption;
  tmpNode2.Attributes['userCreate'] := 'true';
  tmpNode2 := tmpNode2.AddChild('toolbar');
  for cmdButton in Buttons do
  begin
    tmpNode3 := tmpNode2.AddChild('item');
    tmpNode3.Attributes['guidRef'] := cmdButton.Guid;
    tmpNode3 := tmpNode2.AddChild('item');
    tmpNode3.Attributes['guidRef'] := cmdButton.Guid;
    tmpNode3.Attributes['excluded'] := 'true';
  end;

  for cmdButton in Buttons do
  begin
    if cmdButton.Type_ = SCommandButtonType.SCommandButtonTypeSub then
    begin
      subBar := SubCommandBar(cmdButton);
      tmpNode2 := tmpNode1.AddChild('commandBarData');
      tmpNode2.Attributes['guid'] := subBar.FlyoutBarRef;
      tmpNode2.Attributes['type'] := 'toolbar';
      //tmpNode2.Attributes['nonLocalizableName'] := subBar.NonLocalizableName;
      tmpNode2.Attributes['userCaption'] := subBar.Caption;
      tmpNode2.Attributes['userToopTip'] := subBar.ToolTip;
      tmpNode2.Attributes['userCreate'] := 'true';
      tmpNode2 := tmpNode2.AddChild('container');
      for cmdButton1 in subBar.Buttons do
      begin
        tmpNode3 := tmpNode2.AddChild('item');
        tmpNode3.Attributes['guidRef'] := cmdButton1.Guid;
        tmpNode3 := tmpNode2.AddChild('item');
        tmpNode3.Attributes['guidRef'] := cmdButton.Guid;
        tmpNode3.Attributes['excluded'] := 'true';
      end;
    end;
  end;

  tmpNode1 := rootNode.ChildNodes.FindNode('items', '');
  for cmdButton in Buttons do
  begin
    tmpNode2 := tmpNode1.AddChild('itemData');
    case cmdButton.Type_ of
      SCommandButtonTypeNormal:
        begin
          tmpNode2.Attributes['guid'] := cmdButton.Guid;
          tmpNode2.Attributes['dynamicCommand'] := cmdButton.DynamicCommand;
          tmpNode2.Attributes['dynamicCategory'] := 'ab489730-8791-45d2-a825-b78bbe0d6a5d';
          tmpNode2.Attributes['userCaption'] := cmdButton.Caption;
          tmpNode2.Attributes['userToopTip'] := cmdButton.ToolTip;
          if FileExists(cmdButton.IconPath) then
          begin
            tmpNode3 := tmpNode2.AddChild('userSmallBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(cmdButton.IconPath);

            tmpNode3 := tmpNode2.AddChild('userLargeBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(cmdButton.IconPath, 24, 24);
          end;
        end;
      SCommandButtonTypeSegmentation:
        begin
          tmpNode2.Attributes['guidRef'] := '266435b4-6e53-460f-9fa7-f45be187d400';
        end;
      SCommandButtonTypeSub:
        begin
          subBar := SubCommandBar(cmdButton);
          tmpNode2.Attributes['guid'] := subBar.Guid;
          tmpNode2.Attributes['flyoutBarRef'] := subBar.FlyoutBarRef;
          tmpNode2.Attributes['captionRef'] := 'bc6d3e54-d8cd-449a-aa61-cd442d6617eb';
          tmpNode2.Attributes['tooltipRef'] := 'bc6d3e54-d8cd-449a-aa61-cd442d6617eb';
          tmpNode2.Attributes['userCreated'] := 'true';
          tmpNode2.Attributes['userCaption'] := subBar.Caption;
          tmpNode2.Attributes['userToopTip'] := subBar.ToolTip;
          if FileExists(subBar.IconPath) then
          begin
            tmpNode3 := tmpNode2.AddChild('userSmallBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(subBar.IconPath);

            tmpNode3 := tmpNode2.AddChild('userLargeBitmap');
            tmpNode3.Attributes['xmlns:dt'] := 'urn:schemas-microsoft-com:datatypes';
            tmpNode3.Attributes['dt:dt'] := 'bin.base64';
            tmpNode3.Text := TIcon2Base64.GetBase64(subBar.IconPath, 24, 24);
          end;
        end;
    end;
  end;

  tmpXmlDoc.SaveToFile(TPath.GetTempPath + '\x8workspace.xml');

  rs := TResourceStream.Create(HInstance, 'X8Empty2', RT_RCDATA);
  rs.SaveToFile(TPath.GetTempPath + '\ttttmp.xml');
  rs.Destroy;
  tmpXmlDoc := TXMLDocument.Create(mOwner);
  tmpXmlDoc.Active := True;
  tmpXmlDoc.Options := tmpXmlDoc.Options + [doNodeAutoIndent];
  tmpXmlDoc.LoadFromFile(TPath.GetTempPath + '\ttttmp.xml');
  DeleteFile(PWideChar(TPath.GetTempPath + '\ttttmp.xml'));

  rootNode := tmpXmlDoc.DocumentElement;
  tmpNode1 := rootNode.ChildNodes.FindNode('toolbars', '');
  tmpNode1 := tmpNode1.AddChild('toolbar');
  tmpNode1.Attributes['selected'] := 'true';
  tmpNode1.Attributes['guid'] := Guid;
  tmpNode1.Attributes['name'] := Name;

  tmpXmlDoc.SaveToFile(TPath.GetTempPath + '\x8exportsettins.xml');

  rs := TResourceStream.Create(HInstance, 'X8cdws', RT_RCDATA);
  rs.SaveToFile(TPath.GetTempPath + '\x8z.zip');
  rs.Destroy;

  zipFile := TZipFile.Create;
  zipFile.Open(TPath.GetTempPath + '\x8z.zip', zmReadWrite);
  zipFile.Add(TPath.GetTempPath + '\x7workspace.xml', 'content/workspace.xml');
  zipFile.Add(TPath.GetTempPath + '\x7exportsettins.xml', 'content/exportsettings.xml');
  zipFile.Close;

  DeleteFile(PWideChar(TPath.GetTempPath + '\x7workspace.xml'));
  DeleteFile(PWideChar(TPath.GetTempPath + '\x7exportsettins.xml'));

  RenameFile(TPath.GetTempPath + '\x7z.zip', fileName);
end;

procedure SCommandBar.ToX8(fileName: string);
var
  rs: TResourceStream;
  tmpXmlDoc: IXMLDocument;
  rootNode, tmpNode1, tmpNode2, tmpNode3: IXMLNode;
  cmdButton, cmdButton1: SCommandButton;
  subBar: SubCommandBar;
  zipFile: TZipFile;
  wicImage: TWICImage;
  icon: TIcon;
begin
  rs := TResourceStream.Create(HInstance, 'X8Empty1', RT_RCDATA);
  rs.SaveToFile(TPath.GetTempPath + '\ttttmp.xml');
  rs.Destroy;
  tmpXmlDoc := TXMLDocument.Create(mOwner);
  tmpXmlDoc.Active := True;
  tmpXmlDoc.Options := tmpXmlDoc.Options + [doNodeAutoIndent];
  tmpXmlDoc.LoadFromFile(TPath.GetTempPath + '\ttttmp.xml');
  DeleteFile(PWideChar(TPath.GetTempPath + '\ttttmp.xml'));
  rootNode := tmpXmlDoc.DocumentElement;

  tmpNode1 := rootNode.ChildNodes.FindNode('commandBars', '');
  tmpNode2 := tmpNode1.AddChild('commandBarData');
  tmpNode2.Attributes['guid'] := Guid;
  tmpNode2.Attributes['type'] := 'toolbar';
  tmpNode2.Attributes['nonLocalizableName'] := Name;
  tmpNode2.Attributes['userCaption'] := Self.Caption;
  tmpNode2.Attributes['userCreated'] := 'true';
  tmpNode2 := tmpNode2.AddChild('toolbar');
  tmpNode2.Attributes['type'] := 'toolbar';
  tmpNode2.Attributes['dock'] := 'fill';
  tmpNode2.Attributes['itemFace'] := 'notSet';
  for cmdButton in Buttons do
  begin
    tmpNode3 := tmpNode2.AddChild('item');
    tmpNode3.Text := '';
    tmpNode3.Attributes['guidRef'] := cmdButton.Guid;
  end;
  for cmdButton in Buttons do
  begin
    tmpNode3 := tmpNode2.AddChild('item');
    tmpNode3.Text := '';
    tmpNode3.Attributes['guidRef'] := cmdButton.Guid;
    tmpNode3.Attributes['excluded'] := 'true';
  end;

  for cmdButton in Buttons do
  begin
    if cmdButton.Type_ = SCommandButtonType.SCommandButtonTypeSub then
    begin
      subBar := SubCommandBar(cmdButton);
      tmpNode2 := tmpNode1.AddChild('commandBarData');
      tmpNode2.Attributes['guid'] := subBar.FlyoutBarRef;
      tmpNode2.Attributes['type'] := 'toolbar';
      //tmpNode2.Attributes['nonLocalizableName'] := subBar.NonLocalizableName;
      tmpNode2.Attributes['userCaption'] := subBar.Caption;
      tmpNode2.Attributes['userToopTip'] := subBar.ToolTip;
      tmpNode2.Attributes['userCreate'] := 'true';
      tmpNode2 := tmpNode2.AddChild('toolbar');
      for cmdButton1 in subBar.Buttons do
      begin
        tmpNode3 := tmpNode2.AddChild('item');
        tmpNode3.Attributes['guidRef'] := cmdButton1.Guid;
      end;
      for cmdButton1 in subBar.Buttons do
      begin
        tmpNode3 := tmpNode2.AddChild('item');
        tmpNode3.Attributes['guidRef'] := cmdButton.Guid;
        tmpNode3.Attributes['excluded'] := 'true';
      end;
    end;
  end;

  rs := TResourceStream.Create(HInstance, 'X8cdws', RT_RCDATA);
  rs.SaveToFile(TPath.GetTempPath + '\x8z.zip');
  rs.Destroy;
  zipFile := TZipFile.Create;
  zipFile.Open(TPath.GetTempPath + '\x8z.zip', zmReadWrite);
  tmpNode1 := rootNode.ChildNodes.FindNode('items', '');
  for cmdButton in Buttons do
  begin
    tmpNode2 := tmpNode1.AddChild('itemData');
    tmpNode2.Text := '';
    case cmdButton.Type_ of
      SCommandButtonTypeNormal:
        begin
          tmpNode2.Attributes['guid'] := cmdButton.Guid;
          tmpNode2.Attributes['dynamicCommand'] := cmdButton.DynamicCommand;
          tmpNode2.Attributes['dynamicCategory'] := 'ab489730-8791-45d2-a825-b78bbe0d6a5d';
          //tmpNode2.Attributes['userCaption'] := cmdButton.Caption;
          //tmpNode2.Attributes['userToopTip'] := cmdButton.ToolTip;

          if FileExists(cmdButton.IconPath) then
          begin
            tmpNode2.Attributes['icon'] := 'guid://' + cmdButton.IconGuid;
            zipFile.Add(cmdButton.IconPath, 'content/icons/' + cmdButton.IconGuid + '.ico');
          end;
        end;
      SCommandButtonTypeSegmentation:
        begin
          tmpNode2.Attributes['guidRef'] := '266435b4-6e53-460f-9fa7-f45be187d400';
        end;
      SCommandButtonTypeSub:
        begin
          subBar := SubCommandBar(cmdButton);
          tmpNode2.Attributes['guid'] := subBar.Guid;
          tmpNode2.Attributes['flyoutBarRef'] := subBar.FlyoutBarRef;
          tmpNode2.Attributes['captionRef'] := 'bc6d3e54-d8cd-449a-aa61-cd442d6617eb';
          tmpNode2.Attributes['tooltipRef'] := 'bc6d3e54-d8cd-449a-aa61-cd442d6617eb';
          tmpNode2.Attributes['userCreated'] := 'true';
          //tmpNode2.Attributes['userCaption'] := subBar.Caption;
          //tmpNode2.Attributes['userToopTip'] := subBar.ToolTip;
          if FileExists(subBar.IconPath) then
          begin
            if FileExists(cmdButton.IconPath) then
            begin
              tmpNode2.Attributes['icon'] := 'guid://' + cmdButton.IconGuid;
              zipFile.Add(cmdButton.IconPath, 'content/icons/' + cmdButton.IconGuid + '.ico');
            end;
          end;
        end;
    end;
  end;

  tmpXmlDoc.SaveToFile(TPath.GetTempPath + '\x8workspace.xml');

  rs := TResourceStream.Create(HInstance, 'X8Empty2', RT_RCDATA);
  rs.SaveToFile(TPath.GetTempPath + '\ttttmp.xml');
  rs.Destroy;
  tmpXmlDoc := TXMLDocument.Create(mOwner);
  tmpXmlDoc.Active := True;
  tmpXmlDoc.Options := tmpXmlDoc.Options + [doNodeAutoIndent];
  tmpXmlDoc.LoadFromFile(TPath.GetTempPath + '\ttttmp.xml');
  DeleteFile(PWideChar(TPath.GetTempPath + '\ttttmp.xml'));

  rootNode := tmpXmlDoc.DocumentElement;
  tmpNode1 := rootNode.ChildNodes.FindNode('toolbars', '');
  tmpNode2 := tmpNode1.AddChild('toolbar', 0);
  tmpNode2.Attributes['selected'] := 'true';
  tmpNode2.Attributes['guid'] := Guid;
  tmpNode2.Attributes['name'] := Name;

  tmpXmlDoc.SaveToFile(TPath.GetTempPath + '\x8exportsettins.xml');

  zipFile.Add(TPath.GetTempPath + '\x8workspace.xml', 'content/workspace.xml');
  zipFile.Add(TPath.GetTempPath + '\x8exportsettins.xml', 'content/exportsettings.xml');
  zipFile.Close;

  DeleteFile(PWideChar(TPath.GetTempPath + '\x8workspace.xml'));
  DeleteFile(PWideChar(TPath.GetTempPath + '\x8exportsettins.xml'));

  DeleteFile(PWideChar(fileName));
  RenameFile(TPath.GetTempPath + '\x8z.zip', fileName);
end;

procedure SCommandBar.SaveToFile(fileName: string);
var
  I: Integer;
  cmdButton, cmdButton1: SCommandButton;
  subB: SubCommandBar;
  tmpNode: IXMLNode;
  ss: TStrings;
begin
  mMainNode.Attributes['Guid'] := Guid;
  mMainNode.Attributes['Caption'] := Caption;
  mMainNode.Attributes['Name'] := Name;
  //mMainNode.ChildNodes.Clear;
  for I := 0 to mMainNode.ChildNodes.Count - 1 do
  begin
    mMainNode.ChildNodes.Delete(0);
  end;

  //不先保存会出错，YUE
  mXmlDoc.SaveToFile(fileName);
  mXmlDoc := nil;
  mMainNode := nil;

  mXmlDoc := TXMLDocument.Create(mOwner);
  mXmlDoc.Active := True;
  mXmlDoc.Options := mXmlDoc.Options + [doNodeAutoIndent];
  mXmlDoc.LoadFromFile(fileName);
  mMainNode := mXmlDoc.DocumentElement;
  //Exit;
  for cmdButton in Buttons do
  begin
    tmpNode := mMainNode.AddChild('CommandButton', '');
    case cmdButton.Type_ of
      SCommandButtonTypeNormal:
        begin
          tmpNode.Attributes['Type'] := cmdButton.Type_;
          tmpNode.Attributes['Guid'] := cmdButton.Guid;
          tmpNode.Attributes['Command'] := cmdButton.DynamicCommand;
          tmpNode.Attributes['IconPath'] := cmdButton.IconPath;
          tmpNode.Attributes['Caption'] := cmdButton.Caption;
          tmpNode.Attributes['ToolTip'] := cmdButton.ToolTip;
          tmpNode.Attributes['IconGuid'] := cmdButton.IconGuid;
          tmpNode := nil;
        end;
      SCommandButtonTypeSegmentation:
        begin
          tmpNode.Attributes['Type'] := cmdButton.Type_;
          tmpNode := nil;
        end;
      SCommandButtonTypeSub:
        begin
          subB := SubCommandBar(cmdButton);
          tmpNode.Attributes['Type'] := cmdButton.Type_;
          tmpNode.Attributes['Guid'] := cmdButton.Guid;
          tmpNode.Attributes['IconPath'] := cmdButton.IconPath;
          tmpNode.Attributes['Caption'] := cmdButton.Caption;
          tmpNode.Attributes['ToolTip'] := cmdButton.ToolTip;
          tmpNode.Attributes['IconGuid'] := cmdButton.IconGuid;
          tmpNode.Attributes['FlyoutBarRef'] := subB.FlyoutBarRef;
          tmpNode := nil;
          tmpNode := tmpNode.AddChild('CommandButton', '');
          for cmdButton1 in subB.Buttons do
          begin
            case cmdButton1.Type_ of
              SCommandButtonTypeNormal:
                ;
              SCommandButtonTypeSegmentation:
                ;
              SCommandButtonTypeSub:
                ;
            end;
          end;
        end;
    end;
  end;

  mXmlDoc.SaveToFile(fileName);
end;

end.

