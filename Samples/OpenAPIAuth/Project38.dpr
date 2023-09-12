program Project38;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit5 in 'Unit5.pas' {Form5},
  HTTP.Server in '..\..\MiniWebServer\HTTP.Server.pas',
  VK.ExternalAuth in '..\..\Tools\VK.ExternalAuth.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
