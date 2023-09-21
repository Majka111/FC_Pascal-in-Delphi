unit CisloRadku;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  S:string;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
 I:integer;
begin
 S:=Edit1.Text;
 try
   try
     I:=StrToInt(s);
   except
       MessageDlg('It must be integer number', mtError, [mbOK], 0);
   end;
finally
   ModalResult:=mrOK;
end;
end;

end.
