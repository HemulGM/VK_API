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
  VK.Account.ProfileInfo in '..\..\Entity\VK.Account.ProfileInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCaptcha, FormCaptcha);
  Application.Run;
end.
