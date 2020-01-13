program VKAuth;

uses
  Vcl.Forms,
  VKAuth.Main in 'VKAuth.Main.pas' {FormMain},
  VK.API in '..\..\VK.API.pas',
  VK.Types in '..\..\VK.Types.pas',
  VK.Components in '..\..\VK.Components.pas',
  VK.Account.Info in '..\..\Entity\VK.Account.Info.pas',
  VK.Entity in '..\..\VK.Entity.pas',
  VK.Account in '..\..\Entity\VK.Account.pas',
  VK.Handler in '..\..\VK.Handler.pas',
  VK.Captcha in '..\..\Forms\VK.Captcha.pas' {FormCaptcha},
  VK.Account.ProfileInfo in '..\..\Entity\VK.Account.ProfileInfo.pas',
  VK.Account.ActiveOffers in '..\..\Entity\VK.Account.ActiveOffers.pas',
  VK.Account.Counters in '..\..\Entity\VK.Account.Counters.pas',
  VK.Account.PushSettings in '..\..\Entity\VK.Account.PushSettings.pas',
  VK.Structs in '..\..\VK.Structs.pas',
  VK.Auth in '..\..\Entity\VK.Auth.pas',
  VK.Users in '..\..\Entity\VK.Users.pas',
  VK.Users.Types in '..\..\Entity\VK.Users.Types.pas',
  VK.LongPollServer in '..\..\VK.LongPollServer.pas',
  VK.UserEvents in '..\..\VK.UserEvents.pas',
  VK.GroupEvents in '..\..\VK.GroupEvents.pas',
  VK.Wall.Comment in '..\..\Entity\VK.Wall.Comment.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCaptcha, FormCaptcha);
  Application.Run;
end.
