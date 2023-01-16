unit ChatFMX.Frame.Attachment.Money;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, ChatFMX.DM.Res, FMX.Layouts, VK.API,
  System.Messaging, ChatFMX.Frame.Attachment, FMX.Effects, FMX.Ani,
  VK.Entity.Market, VK.Entity.MoneyTransfer, VK.Entity.MoneyRequest,
  VK.Entity.Conversation, VK.Entity.Common.ExtendedList, VK.Entity.Message;

type
  TFrameAttachmentMoney = class(TFrameAttachment)
    LayoutText: TLayout;
    LabelAmount: TLabel;
    LabelComment: TLabel;
    Rectangle1: TRectangle;
    RectangleImage: TRectangle;
    RectangleFrame: TRectangle;
    LayoutButtons: TLayout;
    ButtonHistory: TButton;
    RectangleBG: TRectangle;
  private
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    procedure Fill(Item: TVkMoneyTransfer; Data: TVkEntityExtendedList<TVkMessage>); overload;
    procedure Fill(Item: TVkMoneyRequest; Data: TVkEntityExtendedList<TVkMessage>); overload;
  end;

implementation

uses
  VK.Entity.Common, HGM.FMX.Image, VK.Types, VK.Entity.Profile, VK.Entity.Group;

{$R *.fmx}

{ TFrameAttachmentLink }

constructor TFrameAttachmentMoney.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  RectangleFrame.Visible := True;
  Rectangle1.Visible := False;
end;

procedure TFrameAttachmentMoney.Fill(Item: TVkMoneyTransfer; Data: TVkEntityExtendedList<TVkMessage>);
begin
  if Assigned(Item.Amount) then
    LabelAmount.Text := Item.Amount.Text
  else
    LabelAmount.Text := 'Денежный перевод';

  LabelComment.Text := '';
  if not Item.Comment.IsEmpty then
    LabelComment.Text := Item.Comment
  else
  begin
    if PeerIdIsUser(Item.FromId) then
    begin
      var User: TVkProfile;
      if Data.GetProfileById(Item.FromId, User) then
        LabelComment.Text := 'от ' + User.FullNameAcc;
    end
    else
    begin
      var Group: TVkGroup;
      if Data.GetGroupById(Item.FromId, Group) then
        LabelComment.Text := 'от ' + Group.Name;
    end;
  end;
  RectangleImage.Fill.Bitmap.Bitmap.LoadFromResource('payment_snippet');
  RectangleImage.Fill.Kind := TBrushKind.Bitmap;
  RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
end;

procedure TFrameAttachmentMoney.Fill(Item: TVkMoneyRequest; Data: TVkEntityExtendedList<TVkMessage>);
begin
  if Assigned(Item.Amount) then
    LabelAmount.Text := Item.Amount.Text
  else
    LabelAmount.Text := 'Денежный перевод';

  LabelComment.Text := '';
  if PeerIdIsUser(Item.FromId) then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Item.FromId, User) then
      LabelComment.Text := 'Запрос от ' + User.FullNameAcc;
  end
  else
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Item.FromId, Group) then
      LabelComment.Text := 'Запрос от ' + Group.Name;
  end;
  RectangleImage.Fill.Bitmap.Bitmap.LoadFromResource('payment_request_snippet');
  RectangleImage.Fill.Kind := TBrushKind.Bitmap;
  RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
end;

end.

