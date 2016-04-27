unit frmSelect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseForm, Vcl.StdCtrls;

type
  TfSelect = class(TTBaseForm)
    chk_Size: TCheckBox;
    chk_Type: TCheckBox;
    chk_SLines: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fSelect: TfSelect;

implementation

{$R *.dfm}

end.
