unit ChatFMX.Frame.Attachment.Photo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, VK.Entity.Photo, FMX.Objects, VK.Types, System.Messaging,
  ChatFMX.Frame.Attachment;

type
  TFrameAttachmentPhoto = class(TFrameAttachment)
    Image: TImage;
    procedure FrameResize(Sender: TObject);
  private
    FImageUrl: string;
    FImageFile: string;
    FWasImage: Boolean;
    FMaxWidth: Integer;
    FId: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetMaxWidth(const Value: Integer);
    procedure SetId(const Value: string);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Photo: TVkPhoto);
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property Id: string read FId write SetId;
  end;

implementation

uses
  ChatFMX.PreviewManager, System.Threading, VK.Photos, VK.Entity.Common,
  FMX.Ani;

{$R *.fmx}

{ TFrameAttachmentPhoto }

constructor TFrameAttachmentPhoto.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  FWasImage := False;
end;

destructor TFrameAttachmentPhoto.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentPhoto.Fill(Photo: TVkPhoto);
begin
  Id := Photo.ToStringId;
  MaxWidth := Photo.Sizes.GetMaxSizeOrZero.Width;
  if MaxWidth <= 0 then
    MaxWidth := 1000;
  Height := 100;
  var Size := Photo.Sizes.GetSizeFromHeight(400);
  if Assigned(Size) then
  begin
    Width := Height * (Size.Width / Size.Height);
    if Size.Height < Height then
      Height := Width * (Size.Height / Size.Width);
    FImageUrl := Size.Url;
  end
  else
    Width := 80;
end;

procedure TFrameAttachmentPhoto.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
    Image.Bitmap := nil
  else
  try
    Image.Bitmap.LoadFromFile(FImageFile);
    TAnimator.AnimateFloat(Image, 'Opacity', 1);
    FWasImage := True;
  except
    Image.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentPhoto.FrameResize(Sender: TObject);
begin
  if Width > MaxWidth then
    Width := MaxWidth;
end;

procedure TFrameAttachmentPhoto.SetId(const Value: string);
begin
  FId := Value;
end;

procedure TFrameAttachmentPhoto.SetMaxWidth(const Value: Integer);
begin
  FMaxWidth := Value;
end;

procedure TFrameAttachmentPhoto.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
  begin
    if (not FImageUrl.IsEmpty) and (Image.Bitmap.IsEmpty) then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
    Image.Bitmap := nil;
end;

end.

