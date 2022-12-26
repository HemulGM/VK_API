unit ChatFMX.Frame.Attachment.AudioPlaylist;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, ChatFMX.DM.Res, FMX.Layouts, VK.API,
  System.Messaging, ChatFMX.Frame.Attachment, FMX.Effects, FMX.Ani,
  VK.Entity.Common.ExtendedList, VK.Entity.Message, VK.Entity.Playlist;

type
  TFrameAttachmentAudioPlaylist = class(TFrameAttachment)
    LayoutText: TLayout;
    LabelTitle: TLabel;
    Rectangle1: TRectangle;
    RectangleImage: TRectangle;
    RectangleFrame: TRectangle;
    RectangleBG: TRectangle;
    Rectangle2: TRectangle;
    Layout1: TLayout;
    LabelArtist: TLabel;
    Layout2: TLayout;
    LabelGenre: TLabel;
    LabelYear: TLabel;
    VertScrollBoxPlaylist: TVertScrollBox;
    CircleControl: TCircle;
    PathControl: TPath;
  private
    FGenres: string;
    FArtist: string;
    FTitle: string;
    FYear: Integer;
    FImageUrl: string;
    FImageFile: string;
    procedure SetGenres(const Value: string);
    procedure SetArtist(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetYear(const Value: Integer);
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Item: TVkAudioPlaylist; Data: TVkEntityExtendedList<TVkMessage>); overload;
    property Genres: string read FGenres write SetGenres;
    property Artist: string read FArtist write SetArtist;
    property Title: string read FTitle write SetTitle;
    property Year: Integer read FYear write SetYear;
  end;

implementation

uses
  VK.Entity.Common, HGM.FMX.Image, VK.Types, VK.Entity.Profile, VK.Entity.Group,
  ChatFMX.PreviewManager;

{$R *.fmx}

{ TFrameAttachmentAudioPlaylist }

constructor TFrameAttachmentAudioPlaylist.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  RectangleFrame.Visible := True;
  Rectangle1.Visible := False;
end;

destructor TFrameAttachmentAudioPlaylist.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentAudioPlaylist.Fill(Item: TVkAudioPlaylist; Data: TVkEntityExtendedList<TVkMessage>);
begin
  Genres := Item.Genres.ToStringNames;
  var TmpArtist := '';
  for var ArtistItem in Item.MainArtists do
    TmpArtist := TmpArtist + ArtistItem.Name + ', ';
  Artist := TmpArtist.Trim([',', ' ']);
  Title := Item.Title;
  Year := Item.Year;
  if Assigned(Item.Photo) then
    FImageUrl := Item.Photo.Photo68;
end;

procedure TFrameAttachmentAudioPlaylist.SetArtist(const Value: string);
begin
  FArtist := Value;
  LabelArtist.AutoSize := False;
  LabelArtist.Text := FArtist;
  LabelArtist.AutoSize := True;
  if LabelArtist.Width > 150 then
  begin
    LabelArtist.AutoSize := False;
    LabelArtist.Width := 150;
  end;
end;

procedure TFrameAttachmentAudioPlaylist.SetGenres(const Value: string);
begin
  FGenres := Value;
  LabelGenre.AutoSize := False;
  LabelGenre.Text := FGenres;
  LabelGenre.AutoSize := True;
  if LabelGenre.Width > 150 then
  begin
    LabelGenre.AutoSize := False;
    LabelGenre.Width := 150;
  end;
end;

procedure TFrameAttachmentAudioPlaylist.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

procedure TFrameAttachmentAudioPlaylist.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    RectangleImage.Fill.Bitmap.Bitmap := nil;
end;

procedure TFrameAttachmentAudioPlaylist.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
    RectangleImage.Fill.Bitmap.Bitmap := nil
  else
  try
    RectangleImage.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    RectangleImage.Fill.Kind := TBrushKind.Bitmap;
    RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    RectangleImage.Fill.Bitmap.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentAudioPlaylist.SetYear(const Value: Integer);
begin
  FYear := Value;
  LabelYear.Text := ' · ' + FYear.ToString;
end;

end.

