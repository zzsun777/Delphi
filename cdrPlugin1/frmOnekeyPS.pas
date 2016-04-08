unit frmOnekeyPS;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.StdCtrls,
  VGCore_TLB;

type
  TfOnekeyPS = class(TTBaseForm)
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fOnekeyPS: TfOnekeyPS;

implementation

{$R *.dfm}

procedure TfOnekeyPS.FormCreate(Sender: TObject);
begin
  inherited;
  if StartCheck() then
  begin
    Free;
    Exit;
  end;
  if mApp.ActiveDocument.Selection.Shapes.Count > 1 then
  begin
    MessageBox(self.Handle, '只能选择一个对象！', '错误', MB_OK + MB_ICONSTOP);
    Free;
    Exit;
  end;
  if mApp.ActiveShape.type_ <> cdrBitmapShape then
  begin
    MessageBox(self.Handle, '请选择一个位图！', '错误', MB_OK + MB_ICONSTOP);
    Free;
    Exit;
  end;
end;

end.

