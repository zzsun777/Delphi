unit CUtils;

interface

uses
  Xml.XMLDoc, Xml.XMLIntf, ArrayEx;

type
  cdrVersion = (cdrVersion14, cdrVersion15, cdrVersion16, cdrVersion17, cdrVersion18);

type
  CommandButtonStruct = record
  public
    guid: string;
    dynamicCommand: string;
    userSmallBitmap: string;
    userLargeBitmap: string;
    icon: string;
  end;

type
  CommandBar = class
  private
    mGuid: string;
    mType: string;
    mNonLocalizableName: string;
    mUserCaption: string;
    mButtons: TArrayEx<CommandButtonStruct>;
  private
    function GetGuid: string;
    procedure SetGuid(guid: string);
  public
    property Guid: string read GetGuid write SetGuid;
  public
    constructor Create(guid, type_, name, caption: string);
  end;

type
  XSLTControl = class
  public
    class function CreateEmptyXSLT(version: cdrVersion): TXMLDocument;
  end;

implementation

constructor CommandBar.Create(guid, type_, name, caption: string);
begin
  mButtons := [];
  mGuid := guid;
  mType := type_;
  mNonLocalizableName := name;
  mUserCaption := caption;
end;

function CommandBar.GetGuid;
begin
  Result := mGuid;
end;

procedure CommandBar.SetGuid(guid: string);
begin
  mGuid := guid;
end;

class function XSLTControl.CreateEmptyXSLT(version: cdrVersion): TXMLDocument;
var
  tmpNode: IXMLNode;
begin
  Result := TXMLDocument.Create(nil);
  Result.Active := True;
  Result.Version := '1.0';
  Result.Encoding := 'UTF-8';
  if version < cdrVersion.cdrVersion17 then
  begin
    tmpNode := Result.AddChild('xsl:stylesheet');
    tmpNode.SetAttributeNS('xmlns:xsl', '', 'http://www.w3.org/1999/XSL/Transform');
    tmpNode.SetAttributeNS('xmlns:export', '', 'export');
    tmpNode.SetAttributeNS('version', '', '1.0');
    tmpNode := tmpNode.AddChild('xsl:output');
    tmpNode.SetAttributeNS('method', '', 'xml');
    tmpNode.SetAttributeNS('encoding', '', 'yes');
    tmpNode := tmpNode.AddChild('export:data');
    tmpNode := tmpNode.AddChild('uiConfig');

  end;
end;

end.

