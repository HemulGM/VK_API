unit ChatFMX.Frame.Window.Photo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatFMX.Frame.Window, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,
  ChatFMX.DM.Res, System.Messaging, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Ani;

type
  TFrameWindowPhoto = class(TFrameWindow)
    LayoutContent: TLayout;
    LayoutClient: TLayout;
    RectangleBG: TRectangle;
    LayoutRight: TLayout;
    LayoutFooter: TLayout;
    LayoutComment: TLayout;
    LayoutHead: TLayout;
    RectangleHead: TRectangle;
    LayoutActions: TLayout;
    RectangleActionsBG: TRectangle;
    CircleOwnerImage: TCircle;
    LabelOwnerTitle: TLabel;
    LabelDate: TLabel;
    ButtonLike: TButton;
    ButtonShare: TButton;
    LayoutControls: TLayout;
    LayoutBtnLeft: TLayout;
    LayoutBtnRight: TLayout;
    Image1: TImage;
    Layout3: TLayout;
    Layout4: TLayout;
    Image2: TImage;
    Image: TImage;
    CircleUserImage: TCircle;
    LabelCount: TLabel;
    Memo1: TMemo;
    FloatAnimationLeftOp: TFloatAnimation;
    FloatAnimationRightOp: TFloatAnimation;
    procedure LayoutBtnLeftClick(Sender: TObject);
    procedure LayoutBtnRightClick(Sender: TObject);
    procedure LayoutContentResize(Sender: TObject);
    procedure ButtonLikeApplyStyleLookup(Sender: TObject);
    procedure ImageClick(Sender: TObject);
  private
    FPhotos: TArray<string>;
    FCurrent: Integer;
    FCount: Integer;
    FImageUrl: string;
    FImageFile: string;
    FOwnerImageUrl: string;
    FUserImageUrl: string;
    FOwnerImageFile: string;
    FUserImageFile: string;
    AspectRatio: Single;
    Original: TSizeF;
    FOwnerText: string;
    FDate: TDateTime;
    FCanComment: Boolean;
    FCanRepost: Boolean;
    FLikes: Integer;
    FReposts: Integer;
    procedure ShowPhoto(const Index: Integer);
    procedure SetImageUrl(const Value: string);
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetOwnerText(const Value: string);
    procedure SetOwnerImageUrl(const Value: string);
    procedure FOnReadyImageOwner(const Sender: TObject; const M: TMessage);
    procedure FOnReadyImageUser(const Sender: TObject; const M: TMessage);
    procedure SetUserImageUrl(const Value: string);
    procedure SetDate(const Value: TDateTime);
    procedure SetCount(const Value: Integer);
    procedure SetCurrent(const Value: Integer);
    procedure UpdateCount;
    procedure SetCanComment(const Value: Boolean);
    procedure SetCanRepost(const Value: Boolean);
    procedure SetLikes(const Value: Integer);
    procedure SetReposts(const Value: Integer);
  public
    procedure Fill(Photos: TArray<string>; ACurrent: Integer);
    property ImageUrl: string read FImageUrl write SetImageUrl;
    property OwnerText: string read FOwnerText write SetOwnerText;
    property OwnerImageUrl: string read FOwnerImageUrl write SetOwnerImageUrl;
    property UserImageUrl: string read FUserImageUrl write SetUserImageUrl;
    property Date: TDateTime read FDate write SetDate;
    property Count: Integer read FCount write SetCount;
    property Current: Integer read FCurrent write SetCurrent;
    property CanComment: Boolean read FCanComment write SetCanComment;
    property CanRepost: Boolean read FCanRepost write SetCanRepost;
    property Likes: Integer read FLikes write SetLikes;
    property Reposts: Integer read FReposts write SetReposts;
    destructor Destroy; override;
  end;

var
  FrameWindowPhoto: TFrameWindowPhoto;

implementation

uses
  VK.Entity.Photo, ChatFMX.Utils, VK.Types, VK.Entity.Common,
  ChatFMX.PreviewManager, System.Math, VK.Entity.Profile,
  VK.Entity.Common.ExtendedList, VK.Entity.Group;

{$R *.fmx}

{ TFrameWindowPhoto }

procedure TFrameWindowPhoto.ButtonLikeApplyStyleLookup(Sender: TObject);
var
  Button: TButton absolute Sender;
begin
  if Button.Text.IsEmpty then
    Button.Width := 24 + 3 + 3
  else
  begin
    var LabelText: TLabel;
    if Button.FindStyleResource('text', LabelText) then
    begin
      LabelText.AutoSize := False;
      LabelText.AutoSize := True;
      Button.Width := LabelText.Width + 24 + 6 + 3 + 3;
    end;
  end;
end;

destructor TFrameWindowPhoto.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  TPreview.Instance.Unsubscribe(FOnReadyImageUser);
  TPreview.Instance.Unsubscribe(FOnReadyImageOwner);
  inherited;
end;

procedure TFrameWindowPhoto.Fill(Photos: TArray<string>; ACurrent: Integer);
begin
  FPhotos := Photos;
  Count := Length(FPhotos);
  Current := ACurrent;
  UserImageUrl := VK.UserPhoto50;
  ShowPhoto(FCurrent);
end;

procedure TFrameWindowPhoto.UpdateCount;
begin
  LayoutControls.Visible := FCount > 1;
  if FCount = 1 then
    LabelCount.Text := ''
  else
    LabelCount.Text := Format('%d из %d', [FCurrent + 1, FCount]);
end;

procedure TFrameWindowPhoto.SetCanComment(const Value: Boolean);
begin
  FCanComment := Value;
  LayoutComment.Visible := FCanComment;
end;

procedure TFrameWindowPhoto.SetCanRepost(const Value: Boolean);
begin
  FCanRepost := Value;
  ButtonShare.Visible := FCanRepost;
end;

procedure TFrameWindowPhoto.SetCount(const Value: Integer);
begin
  FCount := Value;
  UpdateCount;
end;

procedure TFrameWindowPhoto.SetCurrent(const Value: Integer);
begin
  FCurrent := Value;
  UpdateCount;
end;

procedure TFrameWindowPhoto.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  LabelDate.Text := HumanDateTime(FDate, True, True);
end;

procedure TFrameWindowPhoto.SetImageUrl(const Value: string);
begin
  FImageUrl := Value;
end;

procedure TFrameWindowPhoto.SetLikes(const Value: Integer);
begin
  FLikes := Value;
  if FLikes > 0 then
    ButtonLike.Text := FLikes.ToString
  else
    ButtonLike.Text := '';
  ButtonLike.NeedStyleLookup;
end;

procedure TFrameWindowPhoto.SetOwnerImageUrl(const Value: string);
begin
  FOwnerImageUrl := Value;
  TPreview.Instance.Subscribe(FOwnerImageUrl, FOnReadyImageOwner);
end;

procedure TFrameWindowPhoto.SetOwnerText(const Value: string);
begin
  FOwnerText := Value;
  LabelOwnerTitle.Text := FOwnerText;
end;

procedure TFrameWindowPhoto.SetReposts(const Value: Integer);
begin
  FReposts := Value;
  if FReposts > 0 then
    ButtonShare.Text := FReposts.ToString
  else
    ButtonShare.Text := '';
  ButtonShare.NeedStyleLookup;
end;

procedure TFrameWindowPhoto.SetUserImageUrl(const Value: string);
begin
  FUserImageUrl := Value;
  TPreview.Instance.Subscribe(FUserImageUrl, FOnReadyImageUser);
end;

procedure TFrameWindowPhoto.FOnReadyImage(const Sender: TObject; const M: TMessage);
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
  except
    Image.Bitmap := nil
  end;
end;

procedure TFrameWindowPhoto.FOnReadyImageOwner(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FOwnerImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImageOwner);
  FOwnerImageFile := Data.Value.FileName;
  if FOwnerImageFile.IsEmpty then
    CircleOwnerImage.Fill.Kind := TBrushKind.Solid
  else
  try
    CircleOwnerImage.Fill.Bitmap.Bitmap.LoadFromFile(FOwnerImageFile);
    CircleOwnerImage.Fill.Kind := TBrushKind.Bitmap;
    CircleOwnerImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    CircleOwnerImage.Fill.Kind := TBrushKind.Solid
  end;
end;

procedure TFrameWindowPhoto.FOnReadyImageUser(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FUserImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImageUser);
  FUserImageFile := Data.Value.FileName;
  if FUserImageFile.IsEmpty then
    CircleUserImage.Fill.Kind := TBrushKind.Solid
  else
  try
    CircleUserImage.Fill.Bitmap.Bitmap.LoadFromFile(FUserImageFile);
    CircleUserImage.Fill.Kind := TBrushKind.Bitmap;
    CircleUserImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    CircleUserImage.Fill.Kind := TBrushKind.Solid
  end;
end;

procedure TFrameWindowPhoto.ImageClick(Sender: TObject);
begin
  HideFrame;
end;

procedure TFrameWindowPhoto.LayoutBtnLeftClick(Sender: TObject);
begin
  var NewIndex := High(FPhotos);
  if FCurrent - 1 >= 0 then
    NewIndex := FCurrent - 1;
  if FCurrent <> NewIndex then
    ShowPhoto(NewIndex);
end;

procedure TFrameWindowPhoto.LayoutBtnRightClick(Sender: TObject);
begin
  var NewIndex := 0;
  if FCurrent + 1 <= High(FPhotos) then
    NewIndex := FCurrent + 1;
  if FCurrent <> NewIndex then
    ShowPhoto(NewIndex);
end;

procedure TFrameWindowPhoto.LayoutContentResize(Sender: TObject);
begin
  inherited;
  var MaxW := LayoutContent.Width;
  var MaxH := LayoutContent.Height;

  var H := Original.Height + LayoutFooter.Height;
  var W := Original.Width + LayoutRight.Width;

  if W > MaxW then
  begin
    var A := H / W;
    W := MaxW;
    H := W * A;
  end;

  if H > MaxH then
  begin
    var A := W / H;
    H := MaxH;
    W := H * A;
  end;

  LayoutClient.Width := Max(W, 910);
  LayoutClient.Height := Max(H, 510);
end;

procedure TFrameWindowPhoto.ShowPhoto(const Index: Integer);
begin
  Current := Index;
  AspectRatio := 1;
  OwnerText := '';
  Date := Now;
  CanComment := False;
  CanRepost := False;
  Likes := 0;
  Reposts := 0;

  Original := TSizeF.Create(0, 0);
  var Items: TVkPhotos;
  if VK.Photos.GetById(Items, [FPhotos[Current]], True, True) then
  try
    if Length(Items.Items) > 0 then
    begin
      var Photo := Items.Items[0];
      CanComment := Photo.CanComment;
      CanRepost := Photo.CanRepost;
      Date := Photo.Date;
      if Assigned(Photo.Likes) then
        Likes := Photo.Likes.Count;
      if Assigned(Photo.Reposts) then
        Reposts := Photo.Reposts.Count;
      if Photo.OwnerId.IsUser then
      begin
        var User: TVkProfile;
        if VK.Users.Get(User, Photo.OwnerId, [TVkExtendedField.Photo50]) then
        try
          OwnerImageUrl := User.Photo50;
          OwnerText := User.FullName;
        finally
          User.Free;
        end;
      end
      else
      begin
        var Group: TVkGroup;
        if VK.Groups.GetById(Group, Abs(Photo.OwnerId), [TVkExtendedField.Photo50]) then
        try
          OwnerImageUrl := Group.Photo50;
          OwnerText := Group.Name;
        finally
          Group.Free;
        end;
      end;

      var Size := Photo.Sizes.GetSizeMaxSum;
      if Assigned(Size) then
      begin
        FImageUrl := Size.Url;
        AspectRatio := Size.Width / Size.Height;
        Original := TSizeF.Create(Size.Width, Size.Height);
        TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
      end;
    end;
  finally
    Items.Free;
  end;
  LayoutContentResize(nil);
end;

end.

