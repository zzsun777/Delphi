unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Xml.XMLDoc,
  Xml.xmldom, Vcl.ComCtrls, Xml.XMLIntf;

type
  TForm1 = class(TForm)
    lst1: TListBox;
    redt1: TRichEdit;
    redt2: TRichEdit;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lst1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
  private
    { Private declarations }
    xmlPath: string;
    xml: TXMLDocument;
    rootNode: IXMLNode;
    procedure Refresh;
  private
    procedure CreateEmptyXSLT(version: Integer; fileName: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  name: string;
  n: IXMLNode;
begin
  name := InputBox('请输入', '请输入图标名称：', '图标');
  n := rootNode.AddChild('key');
  n.SetAttributeNS('name', '', name);
  n.AddChild('small');
  n.AddChild('large');
  Refresh;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  if lst1.ItemIndex < 0 then
    Exit;
  rootNode.ChildNodes.Delete(lst1.ItemIndex);
  Refresh;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  curNode: IXMLNode;
begin
  curNode := rootNode.ChildNodes[lst1.ItemIndex];
  curNode.ChildNodes['small'].Text := redt1.Text;
  curNode.ChildNodes['large'].Text := redt2.Text;
  refresh;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  xml.SaveToFile(xmlPath);
  xml.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  xmlPath := 'D:\360data\重要数据\我的文档\百度云同步盘\vspro\Delphi工程\工具栏图标\图标Base64.xml';
  if not FileExists(xmlPath) then
  begin
    xml := TXMLDocument.Create(self);
    xml.Active := True;
    xml.Version := '1.0';
    xml.Encoding := 'UTF-8';
    xml.AddChild('icons');
    xml.SaveToFile(xmlPath);
    xml.Free;
  end;
  xml := TXMLDocument.Create(self);
  xml.Active := True;
  xml.LoadFromFile(xmlPath);
  rootNode := xml.ChildNodes['icons'];
  Refresh;
end;

procedure TForm1.Refresh;
var
  n: IXMLNode;
  I, J: Integer;
begin
  lst1.Clear;
  for I := 0 to rootNode.childNodes.Count - 1 do
  begin
    n := rootNode.childNodes[I];
    if n.nodeName = 'key' then
    begin
      lst1.Items.Add(string(n.Attributes['name']));
    end;
  end;
end;

procedure TForm1.lst1Click(Sender: TObject);
var
  n: IXMLNode;
  I, J: Integer;
begin
  if lst1.ItemIndex < 0 then
    Exit;
  for I := 0 to rootNode.ChildNodes.Count - 1 do
  begin
    n := rootNode.ChildNodes[I];
    if n.NodeName = 'key' then
    begin
      if string(n.Attributes['name']) = lst1.Items[lst1.ItemIndex] then
      begin
        for J := 0 to n.childNodes.Count - 1 do
        begin
          if n.childNodes[J].nodeName = 'small' then
          begin
            redt1.Text := n.childNodes[J].Text;
          end
          else if n.ChildNodes[J].NodeName = 'large' then
          begin
            redt2.Text := n.childNodes[J].Text;
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm1.CreateEmptyXSLT(version: Integer; fileName: string);
var
  mXml: TXMLDocument;
begin
  mXml := TXMLDocument.Create(self);
  mXml.Active := True;
  mXml.Version := '1.0';
  mXml.Encoding := 'UTF-8';
end;

end.

