unit ChatFMX.Frame.Attachment.WallFwd;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatFMX.Frame.Attachment, VK.Entity.Media, FMX.Controls.Presentation;

type
  TFrameAttachmentWallFwd = class(TFrameAttachment)
    LabelTitle: TLabel;
    procedure LabelTitleMouseEnter(Sender: TObject);
    procedure LabelTitleMouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Fill(Post: TVkPost);
  end;

var
  FrameAttachmentWallFwd: TFrameAttachmentWallFwd;

implementation

{$R *.fmx}

{ TFrameAttachmentWallFwd }

procedure TFrameAttachmentWallFwd.Fill(Post: TVkPost);
begin
  Id := Post.Id;
end;

procedure TFrameAttachmentWallFwd.LabelTitleMouseEnter(Sender: TObject);
begin
  LabelTitle.Font.Style := LabelTitle.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentWallFwd.LabelTitleMouseLeave(Sender: TObject);
begin
  LabelTitle.Font.Style := LabelTitle.Font.Style - [TFontStyle.fsUnderline];
end;

end.

