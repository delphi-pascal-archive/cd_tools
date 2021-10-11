program SRC;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  EraseFrm in 'EraseFrm.pas' {Form2},
  Options in 'Options.pas' {Form3},
  WriteFrm in 'WriteFrm.pas' {Form4},
  AboutFrm in 'AboutFrm.pas' {AboutForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CD\DVD Writer';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
