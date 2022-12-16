unit ChatFMX.Frame.Attachment.Gift;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, FMX.Objects, VK.Types, System.Messaging, VK.Entity.Video, FMX.Layouts,
  FMX.Controls.Presentation, VK.Entity.Gift, VK.Entity.Message,
  ChatFMX.Frame.Attachment;

type
  TFrameAttachmentGift = class(TFrameAttachment)
    LayoutCaption: TLayout;
    PathGift: TPath;
    LabelGift: TLabel;
    LabelText: TLabel;
    FlowLayoutButtons: TFlowLayout;
    RectangleMy: TRectangle;
    LabelMy: TLabel;
    RectangleMore: TRectangle;
    LabelMore: TLabel;
    RectangleSend: TRectangle;
    LabelSend: TLabel;
    LayoutGift: TLayout;
    Rectangle1: TRectangle;
    RectangleGift: TRectangle;
    procedure LabelTextResize(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FImageUrl: string;
    FImageFile: string;
    FStickersProductId: Integer;
    FIsMini: Boolean;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetStickersProductId(const Value: Integer);
    procedure SetIsMini(const Value: Boolean);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Gift: TVkGift; Msg: TVkMessage; CanAnswer: Boolean);
    property StickersProductId: Integer read FStickersProductId write SetStickersProductId;
    property IsMini: Boolean read FIsMini write SetIsMini;
  end;

implementation

uses
  ChatFMX.PreviewManager, HGm.Common.DateUtils, System.Threading,
  VK.Entity.Common, VK.Video;

{$R *.fmx}

{ TFrameAttachmentPhoto }

procedure TFrameAttachmentGift.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
  begin
    if not FImageUrl.IsEmpty then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
  begin
    RectangleGift.Fill.Bitmap.Bitmap := nil;
  end;
end;

constructor TFrameAttachmentGift.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  IsMini := False;
end;

destructor TFrameAttachmentGift.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentGift.Fill(Gift: TVkGift; Msg: TVkMessage; CanAnswer: Boolean);
begin
  LabelText.Text := Msg.Text;
  LabelText.Visible := not LabelText.Text.IsEmpty;
  FImageUrl := Gift.Thumb256;
  StickersProductId := Gift.StickersProductId;
  RectangleMy.Visible := Msg.FromId <> VK.UserId;
  RectangleMore.Visible := CanAnswer and (Msg.FromId = VK.UserId);
  RectangleSend.Visible := CanAnswer and (Msg.FromId <> VK.UserId);
end;

procedure TFrameAttachmentGift.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    RectangleGift.Fill.Kind := TBrushKind.Solid;
  end
  else
  try
    RectangleGift.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    RectangleGift.Fill.Kind := TBrushKind.Bitmap;
    RectangleGift.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    RectangleGift.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TFrameAttachmentGift.FrameResize(Sender: TObject);
begin
  var Sz: Single := 0;
  for var Control in Controls do
    if Control.IsVisible then
      Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  Height := Sz + 20;
end;

procedure TFrameAttachmentGift.LabelTextResize(Sender: TObject);
begin
  FrameResize(nil);
end;

procedure TFrameAttachmentGift.SetIsMini(const Value: Boolean);
begin
  FIsMini := Value;
  LayoutCaption.Visible := not FIsMini;
  case FIsMini of
    True:
      begin
        RectangleGift.Align := TAlignLayout.Left;
        Height := 360;
        FlowLayoutButtons.Justify := TFlowJustify.Left;
        FlowLayoutButtons.JustifyLastLine := TFlowJustify.Left;
      end;
    False:
      begin
        RectangleGift.Align := TAlignLayout.Center;
        Height := 410;
        FlowLayoutButtons.Justify := TFlowJustify.Center;
        FlowLayoutButtons.JustifyLastLine := TFlowJustify.Center;
      end;
  end;
end;

procedure TFrameAttachmentGift.SetStickersProductId(const Value: Integer);
begin
  FStickersProductId := Value;
end;

end.

