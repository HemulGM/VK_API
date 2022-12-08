program ChatFMX;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  ChatFMX.Main in 'ChatFMX.Main.pas' {FormMain},
  ChatFMX.Frame.Chat in 'ChatFMX.Frame.Chat.pas' {FrameChat: TFrame},
  ChatFMX.DM.Res in 'ChatFMX.DM.Res.pas' {DataModuleRes: TDataModule},
  ChatFMX.View.ChatItem in 'ChatFMX.View.ChatItem.pas',
  ChatFMX.PreviewManager in 'ChatFMX.PreviewManager.pas',
  ChatFMX.Utils in 'ChatFMX.Utils.pas',
  ChatFMX.Frame.Message in 'ChatFMX.Frame.Message.pas' {FrameMessage: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TDataModuleRes, DataModuleRes);
  Application.Run;
end.
