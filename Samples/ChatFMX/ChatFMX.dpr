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
  ChatFMX.Frame.Message in 'ChatFMX.Frame.Message.pas' {FrameMessage: TFrame},
  ChatFMX.Frame.Loading in 'ChatFMX.Frame.Loading.pas' {FrameLoading: TFrame},
  ChatFMX.Frame.Attachment.Photo in 'ChatFMX.Frame.Attachment.Photo.pas' {FrameAttachmentPhoto: TFrame},
  ChatFMX.Frame.Attachment.AudioMessage in 'ChatFMX.Frame.Attachment.AudioMessage.pas' {FrameAttachmentAudioMessage: TFrame},
  ChatFMX.Frame.Attachment.Sticker in 'ChatFMX.Frame.Attachment.Sticker.pas' {FrameAttachmentSticker: TFrame},
  ChatFMX.Frame.Attachment.Video in 'ChatFMX.Frame.Attachment.Video.pas' {FrameAttachmentVideo},
  ChatFMX.Frame.MessageAction in 'ChatFMX.Frame.MessageAction.pas' {FrameMessageAction: TFrame},
  ChatFMX.Frame.MessageDate in 'ChatFMX.Frame.MessageDate.pas' {FrameMessageDate: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TDataModuleRes, DataModuleRes);
  Application.Run;
end.
