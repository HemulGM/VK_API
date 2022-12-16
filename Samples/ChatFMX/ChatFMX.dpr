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
  ChatFMX.Frame.MessageDate in 'ChatFMX.Frame.MessageDate.pas' {FrameMessageDate: TFrame},
  ChatFMX.Frame.Attachment.Audio in 'ChatFMX.Frame.Attachment.Audio.pas' {FrameAttachmentAudio},
  ChatFMX.Frame.Attachment.Document in 'ChatFMX.Frame.Attachment.Document.pas' {FrameAttachmentDocument},
  ChatFMX.Frame.Attachment.Geo in 'ChatFMX.Frame.Attachment.Geo.pas' {FrameAttachmentGeo},
  ChatFMX.Frame.Attachment.ReplyMessage in 'ChatFMX.Frame.Attachment.ReplyMessage.pas' {FrameAttachmentReplyMessage: TFrame},
  ChatFMX.Frame.Attachment.Gift in 'ChatFMX.Frame.Attachment.Gift.pas' {FrameAttachmentGift},
  ChatFMX.Frame.Attachment.Message in 'ChatFMX.Frame.Attachment.Message.pas' {FrameAttachmentMessage: TFrame},
  ChatFMX.Frame.Attachment.Messages in 'ChatFMX.Frame.Attachment.Messages.pas' {FrameAttachmentMessages},
  ChatFMX.Frame.Attachment.Link in 'ChatFMX.Frame.Attachment.Link.pas' {FrameAttachmentLink: TFrame},
  ChatFMX.Frame.Attachment.Wall in 'ChatFMX.Frame.Attachment.Wall.pas' {FrameAttachmentWall},
  ChatFMX.Frame.Attachment.Call in 'ChatFMX.Frame.Attachment.Call.pas' {FrameAttachmentCall: TFrame},
  ChatFMX.Frame.Attachment in 'ChatFMX.Frame.Attachment.pas' {FrameAttachment: TFrame},
  ChatFMX.Frame.Attachment.WallFwd in 'ChatFMX.Frame.Attachment.WallFwd.pas' {FrameAttachmentWallFwd: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TDataModuleRes, DataModuleRes);
  Application.Run;
end.
