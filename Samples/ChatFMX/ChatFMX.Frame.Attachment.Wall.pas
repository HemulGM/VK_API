unit ChatFMX.Frame.Attachment.Wall;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Memo.Types, FMX.Objects, FMX.Ani,
  FMX.ScrollBox, FMX.Memo, VK.API, VK.Entity.Message, VK.Entity.Conversation,
  ChatFMX.Frame.Attachment.Photo, ChatFMX.Frame.Attachment.Video,
  System.Messaging, VK.Entity.Media, ChatFMX.Frame.Attachment.Document,
  ChatFMX.Frame.Attachment.Audio, ChatFMX.Frame.Attachment.AudioMessage,
  ChatFMX.Frame.Attachment, VK.Entity.Geo, VK.Entity.Common.ExtendedList;

type
  TFrameAttachmentWall = class(TFrameAttachment)
    LayoutContent: TLayout;
    LayoutClient: TLayout;
    MemoText: TMemo;
    FlowLayoutMedia: TFlowLayout;
    LayoutAutor: TLayout;
    LabelAutor: TLabel;
    LayoutHead: TLayout;
    CircleAvatar: TCircle;
    LayoutFrom: TLayout;
    LabelFrom: TLabel;
    LayoutDetails: TLayout;
    LabelTime: TLabel;
    LineLeft: TLine;
    Path1: TPath;
    LayoutDeleted: TLayout;
    RectangleDeleted: TRectangle;
    LayoutDeletedContent: TLayout;
    PathDeleted: TPath;
    LabelDeleteReason: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure MemoTextResize(Sender: TObject);
    procedure MemoTextChange(Sender: TObject);
    procedure LabelFromMouseEnter(Sender: TObject);
    procedure LabelFromMouseLeave(Sender: TObject);
  private
    FFromText: string;
    FImageUrl: string;
    FImageFile: string;
    FText: string;
    FDate: TDateTime;
    FAutor: string;
    procedure SetFromText(const Value: string);
    procedure SetImageUrl(const Value: string);
    procedure SetText(const Value: string);
    procedure ClearMedia;
    procedure RecalcMedia;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetDate(const Value: TDateTime);
    procedure SetAutor(const Value: string);
    procedure CreatePhotos(Items: TVkAttachmentArray);
    procedure CreateVideos(Items: TVkAttachmentArray);
    procedure CreateAudios(Items: TVkAttachmentArray);
    procedure CreateDocs(Items: TVkAttachmentArray);
    procedure CreatePosts(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
    procedure CreateLinks(Items: TVkAttachmentArray);
    procedure CreateCopyHistory(Items: TArray<TVkPost>; Data: TVkEntityExtendedList<TVkMessage>);
    procedure CreateAlbums(Items: TVkAttachmentArray);
    procedure CreateMarket(Items: TVkAttachmentArray);
    procedure CreateMoney(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
    procedure CreateGeo(Value: TVkGeoWall);
    procedure FOnPhotosClick(Sender: TObject);
    function CollectPhotosFrom(const PhotoId: string; out Items: TArray<string>; out Index: Integer): Boolean;
    procedure CreateGraffiti(Items: TVkAttachmentArray);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Item: TVkPost; Data: TVkEntityExtendedList<TVkMessage>);
    property Text: string read FText write SetText;
    property Date: TDateTime read FDate write SetDate;
    property FromText: string read FFromText write SetFromText;
    property ImageUrl: string read FImageUrl write SetImageUrl;
    property Autor: string read FAutor write SetAutor;
  end;

implementation

uses
  System.Math, FMX.Memo.Style, ChatFMX.PreviewManager, VK.Types,
  VK.Entity.Profile, VK.Entity.Group, ChatFMX.Utils,
  ChatFMX.Frame.Attachment.Messages, ChatFMX.Frame.Attachment.Link,
  ChatFMX.Frame.Attachment.WallFwd, ChatFMX.Frame.Attachment.Album,
  ChatFMX.Frame.Attachment.Market, ChatFMX.Frame.Attachment.Money,
  ChatFMX.Frame.Attachment.Geo, ChatFMX.Frame.Window.Photo,
  ChatFMX.Frame.Attachment.Graffiti;

{$R *.fmx}

{ TFrameAttachmentMessage }

procedure TFrameAttachmentWall.SetVisibility(const Value: Boolean);
begin
  inherited;
  for var Control in LayoutClient.Controls do
    if Control is TFrameAttachment then
      (Control as TFrameAttachment).SetVisibility(Value);
  for var Control in FlowLayoutMedia.Controls do
    if Control is TFrameAttachment then
      (Control as TFrameAttachment).SetVisibility(Value);
end;

constructor TFrameAttachmentWall.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  {$IFDEF ANDROID}
  CircleAvatar.Margins.Right := 7;
  {$ENDIF}
  //RectangleBG.Visible := False;
  MemoText.DisableDisappear := True;
  ClearMedia;
end;

procedure TFrameAttachmentWall.ClearMedia;
begin
  FlowLayoutMedia.BeginUpdate;
  try
    while FlowLayoutMedia.ControlsCount > 0 do
      FlowLayoutMedia.Controls[0].Free;
  finally
    FlowLayoutMedia.EndUpdate;
  end;
  RecalcMedia;
end;

procedure TFrameAttachmentWall.RecalcMedia;
begin
  var H: Single := 0;
  for var Control in FlowLayoutMedia.Controls do
  begin
    if (Control.Width > FlowLayoutMedia.Width) or
      ((FlowLayoutMedia.ControlsCount = 1) and (
      (FlowLayoutMedia.Controls[0] is TFrameAttachmentPhoto) or
      (FlowLayoutMedia.Controls[0] is TFrameAttachmentVideo)))
      then
    begin
      var D := Control.Height / Control.Width;
      Control.Width := FlowLayoutMedia.Width;
      Control.Height := Control.Width * D;
    end;
    H := Max(Control.Position.Y + Control.Height, H);
  end;
  if FlowLayoutMedia.Height <> H then
  begin
    FlowLayoutMedia.Height := H;
    FrameResize(nil);
  end;
end;

destructor TFrameAttachmentWall.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentWall.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
    CircleAvatar.Fill.Kind := TBrushKind.Solid
  else
  try
    CircleAvatar.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    CircleAvatar.Fill.Kind := TBrushKind.Bitmap;
    CircleAvatar.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    CircleAvatar.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TFrameAttachmentWall.FrameResize(Sender: TObject);
begin
  var Sz: Single := LayoutContent.Padding.Top + LayoutContent.Padding.Bottom;
  RecalcMedia;
  MemoTextChange(nil);
  for var Control in LayoutClient.Controls do
    if Control.IsVisible then
      Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  Sz := Max(Sz, LayoutHead.Height);
  if Height <> Sz then
  begin
    Height := Sz;
    if Assigned(ParentControl) then
      ParentControl.RecalcSize;
  end;
end;

procedure TFrameAttachmentWall.LabelFromMouseEnter(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.TextSettings.Font.Style := Control.TextSettings.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentWall.LabelFromMouseLeave(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.TextSettings.Font.Style := Control.TextSettings.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentWall.MemoTextChange(Sender: TObject);
begin
  var H := MemoText.ContentSize.Size.Height + 5;
  if H <> MemoText.Height then
  begin
    MemoText.Height := H;
    FrameResize(nil);
  end;
end;

procedure TFrameAttachmentWall.MemoTextResize(Sender: TObject);
begin
  MemoTextChange(Sender);
end;

procedure TFrameAttachmentWall.Fill(Item: TVkPost; Data: TVkEntityExtendedList<TVkMessage>);
begin
  Text := Item.Text;
  Date := Item.Date;
  FromText := Item.FromId.ToString;
  ImageUrl := '';
  Autor := '';
  LayoutDeleted.Visible := False;

  if PeerIdIsUser(Item.FromId) then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Item.FromId, User) then
    begin
      FromText := User.FullName;
      ImageUrl := User.Photo50;
    end;
  end
  else
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Item.FromId, Group) then
    begin
      FromText := Group.Name;
      ImageUrl := Group.Photo50;
    end;
  end;

  if Item.IsDeleted then
  begin
    LayoutDeleted.Visible := True;
    LabelDeleteReason.Text := Item.DeletedReason;
    Text := '';
    RecalcSize;
    Exit;
  end;

  if PeerIdIsUser(Item.SignerId) then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Item.SignerId, User) then
    begin
      Autor := User.FullName;
    end;
  end
  else
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Item.SignerId, Group) then
    begin
      Autor := Group.Name;
    end;
  end;

  if Length(Item.Attachments) > 0 then
  begin
    CreatePhotos(Item.Attachments);
    CreateVideos(Item.Attachments);
    CreateAudios(Item.Attachments);
    CreateAlbums(Item.Attachments);
    CreateDocs(Item.Attachments);
    CreateLinks(Item.Attachments);
    CreatePosts(Item.Attachments, Data);
    CreateMarket(Item.Attachments);
    CreateMoney(Item.Attachments, Data);
    CreateGraffiti(Item.Attachments);
    RecalcMedia;
  end;

  if Length(Item.CopyHistory) > 0 then
    CreateCopyHistory(Item.CopyHistory, Data);

  if Assigned(Item.Geo) then
    CreateGeo(Item.Geo);

  RecalcSize;
end;

procedure TFrameAttachmentWall.CreateGeo(Value: TVkGeoWall);
begin
  var Frame := TFrameAttachmentGeo.Create(LayoutClient, VK);
  Frame.Parent := LayoutClient;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.Fill(Value);
end;

procedure TFrameAttachmentWall.CreateCopyHistory(Items: TArray<TVkPost>; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
  begin
    var Frame := TFrameAttachmentWall.Create(LayoutClient, VK);
    Frame.Parent := LayoutClient;
    Frame.Position.Y := 10000;
    Frame.Align := TAlignLayout.Top;
    Frame.Fill(Item, Data);
  end;
end;

procedure TFrameAttachmentWall.CreateLinks(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Link then
    begin
      var Frame := TFrameAttachmentLink.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Link);
    end;
end;

procedure TFrameAttachmentWall.CreatePosts(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Wall then
    begin
      var Frame := TFrameAttachmentWall.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Wall, Data);
    end;
end;

procedure TFrameAttachmentWall.CreateDocs(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if (Item.&Type = TVkAttachmentType.Doc) and (not Assigned(Item.Doc.Preview)) then
    begin
      var Frame := TFrameAttachmentDocument.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Doc, False);
    end;
end;

procedure TFrameAttachmentWall.CreateAudios(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Audio then
    begin
      var Frame := TFrameAttachmentAudio.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Audio);
    end;
end;

procedure TFrameAttachmentWall.CreateVideos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Video then
    begin
      var Frame := TFrameAttachmentVideo.Create(FlowLayoutMedia, VK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Video);
    end;
end;

procedure TFrameAttachmentWall.CreateMoney(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
    if Item.&Type in [TVkAttachmentType.MoneyTransfer, TVkAttachmentType.MoneyRequest] then
    begin
      var Frame := TFrameAttachmentMoney.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      case Item.&Type of
        TVkAttachmentType.MoneyTransfer:
          Frame.Fill(Item.MoneyTransfer, Data);
        TVkAttachmentType.MoneyRequest:
          Frame.Fill(Item.MoneyRequest, Data);
      end;
    end;
end;

procedure TFrameAttachmentWall.CreateMarket(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Market then
    begin
      var Frame := TFrameAttachmentMarket.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Market);
    end;
end;

procedure TFrameAttachmentWall.CreateAlbums(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Album then
    begin
      var Frame := TFrameAttachmentAlbum.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Album);
    end;
end;

procedure TFrameAttachmentWall.CreatePhotos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
  begin
    if Item.&Type = TVkAttachmentType.Photo then
    begin
      var Frame := TFrameAttachmentPhoto.Create(FlowLayoutMedia, VK);
      Frame.Parent := FlowLayoutMedia;
      Frame.OnClick := FOnPhotosClick;
      Frame.Fill(Item.Photo);
    end;
    if (Item.&Type = TVkAttachmentType.Doc) and (Assigned(Item.Doc.Preview)) then
    begin
      var Frame := TFrameAttachmentDocument.Create(FlowLayoutMedia, VK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Doc, True);
    end;
  end;
end;

procedure TFrameAttachmentWall.FOnPhotosClick(Sender: TObject);
var
  Frame: TFrameAttachmentPhoto absolute Sender;
begin
  if not (Sender is TFrameAttachmentPhoto) then
    Exit;

  var Items: TArray<string>;
  var CurrentIndex: Integer;
  if CollectPhotosFrom(Frame.Id, Items, CurrentIndex) then
  begin
    var Form := Application.MainForm;
    with TFrameWindowPhoto.Create(Form, VK) do
    begin
      Parent := Form;
      Align := TAlignLayout.Contents;
      Fill(Items, CurrentIndex);
      ShowFrame;
    end;
  end;
end;

procedure TFrameAttachmentWall.CreateGraffiti(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Graffiti then
    begin
      var Frame := TFrameAttachmentGraffiti.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Graffiti);
    end;
end;

function TFrameAttachmentWall.CollectPhotosFrom(const PhotoId: string; out Items: TArray<string>; out Index: Integer): Boolean;
begin
  SetLength(Items, FlowLayoutMedia.ControlsCount);
  Index := 0;
  if Length(Items) <= 0 then
    Exit(False);
  var i := 0;
  for var Control in FlowLayoutMedia.Controls do
    if Control is TFrameAttachmentPhoto then
    begin
      var Id :=(Control as TFrameAttachmentPhoto).Id;
      Items[i] := Id;
      if Id = PhotoId then
        Index := i;
      Inc(i);
    end;
  SetLength(Items, i);
  Result := True;
end;

procedure TFrameAttachmentWall.SetAutor(const Value: string);
begin
  FAutor := Value;
  if not FAutor.IsEmpty then
  begin
    LayoutAutor.Visible := True;
    LabelAutor.Text := FAutor;
  end
  else
    LayoutAutor.Visible := False;
end;

procedure TFrameAttachmentWall.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  LabelTime.Text := HumanDateTime(FDate, True, True);
  LabelTime.Hint := FormatDateTime('c', FDate);
end;

procedure TFrameAttachmentWall.SetFromText(const Value: string);
begin
  FFromText := Value;
  LabelFrom.Text := FFromText;
end;

procedure TFrameAttachmentWall.SetImageUrl(const Value: string);
begin
  FImageUrl := Value;
  if not FImageUrl.IsEmpty then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    CircleAvatar.Fill.Kind := TBrushKind.Solid;
end;

procedure TFrameAttachmentWall.SetText(const Value: string);
begin
  FText := ParseMention(Value);
  MemoText.Visible := not FText.IsEmpty;
  if not FText.IsEmpty then
  begin
    MemoText.Text := FText;
    (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
    (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  end;
end;

end.

