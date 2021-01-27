program VKAuth;

uses
  Vcl.Forms,
  VKAuth.Main in 'VKAuth.Main.pas' {FormMain},
  VK.Entity.Common.ExtendedList in '..\..\Entity\VK.Entity.Common.ExtendedList.pas',
  VK.Entity.Group.Invites in '..\..\Entity\VK.Entity.Group.Invites.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
