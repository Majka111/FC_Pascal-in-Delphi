program PASCAL_FC_P_A;

uses
  Vcl.Forms,
  PASCAL_FC_A in 'PASCAL_FC_A.pas' {Form1},
  CisloRadku in 'CisloRadku.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
