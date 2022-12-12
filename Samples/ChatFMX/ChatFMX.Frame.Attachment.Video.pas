unit ChatFMX.Frame.Attachment.Video;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, FMX.Objects, VK.Types, System.Messaging, VK.Entity.Video, FMX.Layouts,
  FMX.Controls.Presentation;

type
  TFrameAttachmentVideo = class(TFrame)
    Image: TImage;
    LayoutInfo: TLayout;
    RectangleTime: TRectangle;
    LayoutInfoB: TLayout;
    LabelTime: TLabel;
    PathPlay: TPath;
    procedure LabelTimeResize(Sender: TObject);
  private
    FVK: TCustomVK;
    FImageUrl: string;
    FImageFile: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Video: TVkVideo);
  end;

implementation

uses
  ChatFMX.PreviewManager, HGm.Common.DateUtils, System.Threading,
  VK.Entity.Common, VK.Video;

{$R *.fmx}

{ TFrameAttachmentPhoto }

constructor TFrameAttachmentVideo.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
end;

destructor TFrameAttachmentVideo.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentVideo.Fill(Video: TVkVideo);
var
  Id: string;
begin
  Id := Video.ToStringId;
  LabelTime.Text := SecondsToMinFormat(Video.Duration);
  Height := 100;
  FImageUrl := '';
  for var Image in Video.Image do
    if Image.Width > 100 then
    begin
      FImageUrl := Image.Url;
      Width := Height * (Image.Width / Image.Height);
    end;
  if FImageUrl.IsEmpty and (Length(Video.Image) > 0) then
  begin
    var Image := Video.Image[High(Video.Image)];
    FImageUrl := Image.Url;
    Width := Height * (Image.Width / Image.Height);
  end;

  if not FImageUrl.IsEmpty then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    Width := 80;
end;

procedure TFrameAttachmentVideo.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    //CircleAvatar.Fill.Kind := TBrushKind.Solid;
    Image.Bitmap := nil;
  end
  else
  try
    Image.Bitmap.LoadFromFile(FImageFile);
    //CircleAvatar.Fill.Kind := TBrushKind.Bitmap;
    //CircleAvatar.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    Image.Bitmap := nil;
    //CircleAvatar.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TFrameAttachmentVideo.LabelTimeResize(Sender: TObject);
begin
  RectangleTime.Width := LabelTime.Width + 5 + PathPlay.Width + RectangleTime.Padding.Left + RectangleTime.Padding.Right;
end;

end.

