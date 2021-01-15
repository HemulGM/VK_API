program SQLiteWork;

uses
  System.StartUpCopy,
  FMX.Forms,
  Sample.SqliteWork in 'Sample.SqliteWork.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
