unit ChatFMX.Frame.Attachment.Gift;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, FMX.Objects, VK.Types, System.Messaging, VK.Entity.Video, FMX.Layouts,
  FMX.Controls.Presentation, VK.Entity.Gift, VK.Entity.Message;

type
  TFrameAttachmentGift = class(TFrame)
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
    FVK: TCustomVK;
    FImageUrl: string;
    FImageFile: string;
    FStickersProductId: Integer;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetStickersProductId(const Value: Integer);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Gift: TVkGift; Msg: TVkMessage; CanAnswer: Boolean);
    property StickersProductId: Integer read FStickersProductId write SetStickersProductId;
  end;

implementation

uses
  ChatFMX.PreviewManager, HGm.Common.DateUtils, System.Threading,
  VK.Entity.Common, VK.Video;

{$R *.fmx}

{ TFrameAttachmentPhoto }

constructor TFrameAttachmentGift.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
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
  RectangleMy.Visible := Msg.FromId <> FVK.UserId;
  RectangleMore.Visible := CanAnswer and (Msg.FromId = FVK.UserId);
  RectangleSend.Visible := CanAnswer and (Msg.FromId <> FVK.UserId);

  if not FImageUrl.IsEmpty then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
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

procedure TFrameAttachmentGift.SetStickersProductId(const Value: Integer);
begin
  FStickersProductId := Value;
end;

end.

