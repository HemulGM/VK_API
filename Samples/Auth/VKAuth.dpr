program VKAuth;

uses
  Vcl.Forms,
  VKAuth.Main in 'VKAuth.Main.pas' {FormMain},
  VK.API in '..\..\VK.API.pas',
  VK.Types in '..\..\VK.Types.pas',
  VK.Components in '..\..\VK.Components.pas',
  VK.Entity.AccountInfo in '..\..\Entity\VK.Entity.AccountInfo.pas',
  VK.Controller in '..\..\VK.Controller.pas',
  VK.Handler in '..\..\VK.Handler.pas',
  VK.Captcha in '..\..\Forms\VK.Captcha.pas' {FormCaptcha},
  VK.Entity.ProfileInfo in '..\..\Entity\VK.Entity.ProfileInfo.pas',
  VK.Entity.ActiveOffers in '..\..\Entity\VK.Entity.ActiveOffers.pas',
  VK.Entity.Counters in '..\..\Entity\VK.Entity.Counters.pas',
  VK.Entity.PushSettings in '..\..\Entity\VK.Entity.PushSettings.pas',
  VK.Structs in '..\..\VK.Structs.pas',
  VK.Entity.User in '..\..\Entity\VK.Entity.User.pas',
  VK.LongPollServer in '..\..\VK.LongPollServer.pas',
  VK.UserEvents in '..\..\VK.UserEvents.pas',
  VK.GroupEvents in '..\..\VK.GroupEvents.pas',
  VK.Entity.Comment in '..\..\Entity\VK.Entity.Comment.pas',
  VK.Entity.Post in '..\..\Entity\VK.Entity.Post.pas',
  VK.Entity.Photo in '..\..\Entity\VK.Entity.Photo.pas',
  VK.Entity.Common in '..\..\Entity\VK.Entity.Common.pas',
  VK.Entity.Link in '..\..\Entity\VK.Entity.Link.pas',
  VK.Entity.Media in '..\..\Entity\VK.Entity.Media.pas',
  VK.Account in '..\..\Controllers\VK.Account.pas',
  VK.Auth in '..\..\Controllers\VK.Auth.pas',
  VK.Users in '..\..\Controllers\VK.Users.pas',
  VK.Messages in '..\..\Controllers\VK.Messages.pas',
  VK.Entity.Keyboard in '..\..\Entity\VK.Entity.Keyboard.pas',
  VK.Entity.Message in '..\..\Entity\VK.Entity.Message.pas',
  VK.OAuth2 in '..\..\Forms\VK.OAuth2.pas' {FormOAuth2},
  VK.Utils in '..\..\VK.Utils.pas',
  VK.Entity.AudioMessage in '..\..\Entity\VK.Entity.AudioMessage.pas',
  VK.Entity.Sticker in '..\..\Entity\VK.Entity.Sticker.pas',
  VK.Entity.Gift in '..\..\Entity\VK.Entity.Gift.pas',
  VK.Entity.Market in '..\..\Entity\VK.Entity.Market.pas',
  VK.Entity.Doc in '..\..\Entity\VK.Entity.Doc.pas',
  VK.Entity.Audio in '..\..\Entity\VK.Entity.Audio.pas',
  VK.Entity.Video in '..\..\Entity\VK.Entity.Video.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCaptcha, FormCaptcha);
  Application.CreateForm(TFormOAuth2, FormOAuth2);
  Application.Run;
end.
