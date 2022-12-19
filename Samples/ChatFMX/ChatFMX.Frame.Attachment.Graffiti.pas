unit ChatFMX.Frame.Attachment.Graffiti;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, VK.API, System.Messaging, ChatFMX.Frame.Attachment,
  VK.Entity.Graffiti;

type
  TFrameAttachmentGraffiti = class(TFrameAttachment)
    Image: TImage;
    procedure FrameResize(Sender: TObject);
  private
    FImageUrl, FImageFile: string;
    FOriginal: TSizeF;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetOriginal(const Value: TSizeF);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Item: TVkGraffiti);
    property Original: TSizeF read FOriginal write SetOriginal;
  end;

implementation

uses
  ChatFMX.PreviewManager, System.Math;

{$R *.fmx}

{ TFrameAttachmentSticker }

procedure TFrameAttachmentGraffiti.SetOriginal(const Value: TSizeF);
begin
  FOriginal := Value;
end;

procedure TFrameAttachmentGraffiti.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    Image.Bitmap := nil;
end;

constructor TFrameAttachmentGraffiti.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
end;

destructor TFrameAttachmentGraffiti.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentGraffiti.Fill(Item: TVkGraffiti);
begin
  FImageUrl := Item.Url;
  Original := TSizeF.Create(Item.Width, Item.Height);
  FrameResize(nil);
end;

procedure TFrameAttachmentGraffiti.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    Image.Bitmap := nil;
  end
  else
  try
    Image.Bitmap.LoadFromFile(FImageFile);
  except
    Image.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentGraffiti.FrameResize(Sender: TObject);
begin
  Height := Min(Width * (Original.Height / Original.Width), 400);
  Width := Height * (Original.Width / Original.Height);
  Image.Width := Width;
end;

end.

