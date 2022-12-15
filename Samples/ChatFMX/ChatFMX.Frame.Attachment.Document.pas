unit ChatFMX.Frame.Attachment.Document;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, VK.API, VK.Entity.Doc,
  VK.Types, System.Messaging;

type
  TFrameAttachmentDocument = class(TFrame)
    LabelSize: TLabel;
    ImageIcon: TImage;
    LayoutContent: TLayout;
    LabelTitle: TLabel;
    LayoutGeneral: TLayout;
    LayoutPreview: TLayout;
    ImagePreview: TImage;
    LayoutInfoB: TLayout;
    RectangleSize: TRectangle;
    LabelTypeSize: TLabel;
    RectangleOver: TRectangle;
    procedure LabelTitleMouseLeave(Sender: TObject);
    procedure LabelTitleMouseEnter(Sender: TObject);
    procedure LabelTypeSizeResize(Sender: TObject);
  private
    FVK: TCustomVK;
    FTitle: string;
    FDocSize: string;
    FDocType: TVkDocumentType;
    FModePreview: Boolean;
    FImageUrl: string;
    FImageFile: string;
    FExt: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetTitle(const Value: string);
    procedure SetDocSize(const Value: string);
    procedure SetDocType(const Value: TVkDocumentType);
    procedure SetModePreview(const Value: Boolean);
    procedure SetExt(const Value: string);
    procedure UpdatePreviewSize;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    procedure Fill(Document: TVkDocument; AsPreview: Boolean);
    property Title: string read FTitle write SetTitle;
    property DocSize: string read FDocSize write SetDocSize;
    property DocType: TVkDocumentType read FDocType write SetDocType;
    property ModePreview: Boolean read FModePreview write SetModePreview;
    property Ext: string read FExt write SetExt;
  end;

var
  IconDoc: TRectF = (
    Top: 0; Bottom: -180
  );
  IconArchive: TRectF = (
    Top: -30; Bottom: -150
  );
  IconImage: TRectF = (
    Top: -60; Bottom: -120
  );
  IconAudio: TRectF = (
    Top: -90; Bottom: -90
  );
  IconVideo: TRectF = (
    Top: -120; Bottom: -60
  );
  IconBook: TRectF = (
    Top: -150; Bottom: -30
  );
  IconFile: TRectF = (
    Top: -180; Bottom: 0
  );

implementation

uses
  ChatFMX.PreviewManager, VK.Entity.Common;

{$R *.fmx}

constructor TFrameAttachmentDocument.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
  ImageIcon.BitmapMargins.Rect := IconAudio;
  ModePreview := False;
end;

procedure TFrameAttachmentDocument.Fill(Document: TVkDocument; AsPreview: Boolean);
begin
  ModePreview := AsPreview;
  FImageUrl := '';
  DocSize := Document.SizeStr;
  if AsPreview then
  begin
    Ext := Document.Ext;
    if Assigned(Document.Preview) and Assigned(Document.Preview.Photo) then
    begin
      var Size := Document.Preview.Photo.Sizes.GetSizeFromHeight(100);
      if Assigned(Size) then
      begin
        Height := 100;
        Width := Height * (Size.Width / Size.Height);
        FImageUrl := Size.Src;
        if not FImageUrl.IsEmpty then
          TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
      end;
    end;
  end
  else
  begin
    Title := Document.Title;
    DocType := Document.&Type;
  end;
end;

procedure TFrameAttachmentDocument.FOnReadyImage(const Sender: TObject; const M: TMessage);
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
    ImagePreview.Bitmap := nil;
  end
  else
  try
    ImagePreview.Bitmap.LoadFromFile(FImageFile);
    //CircleAvatar.Fill.Kind := TBrushKind.Bitmap;
    //CircleAvatar.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    ImagePreview.Bitmap := nil;
    //CircleAvatar.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TFrameAttachmentDocument.UpdatePreviewSize;
begin
  LabelTypeSize.Text := Ext.ToUpper + ' · ' + FDocSize;
end;

procedure TFrameAttachmentDocument.LabelTitleMouseEnter(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.Font.Style := Control.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentDocument.LabelTitleMouseLeave(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.Font.Style := Control.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentDocument.LabelTypeSizeResize(Sender: TObject);
begin
  RectangleSize.Width := LabelTypeSize.Width + RectangleSize.Padding.Left + RectangleSize.Padding.Right;
end;

procedure TFrameAttachmentDocument.SetDocSize(const Value: string);
begin
  FDocSize := Value;
  LabelSize.Text := FDocSize;
  UpdatePreviewSize;
end;

procedure TFrameAttachmentDocument.SetDocType(const Value: TVkDocumentType);
begin
  FDocType := Value;
  case FDocType of
    TVkDocumentType.None, TVkDocumentType.Unknown:
      ImageIcon.BitmapMargins.Rect := IconFile;
    TVkDocumentType.Text:
      ImageIcon.BitmapMargins.Rect := IconDoc;
    TVkDocumentType.Archive:
      ImageIcon.BitmapMargins.Rect := IconArchive;
    TVkDocumentType.GIF, TVkDocumentType.Picture:
      ImageIcon.BitmapMargins.Rect := IconImage;
    TVkDocumentType.Audio:
      ImageIcon.BitmapMargins.Rect := IconAudio;
    TVkDocumentType.Video:
      ImageIcon.BitmapMargins.Rect := IconVideo;
    TVkDocumentType.Book:
      ImageIcon.BitmapMargins.Rect := IconBook;
  end;
end;

procedure TFrameAttachmentDocument.SetExt(const Value: string);
begin
  FExt := Value;
  UpdatePreviewSize;
end;

procedure TFrameAttachmentDocument.SetModePreview(const Value: Boolean);
begin
  FModePreview := Value;
  LayoutGeneral.Visible := not FModePreview;
  LayoutPreview.Visible := FModePreview;
  if FModePreview then
  begin
    Padding.Top := 0;
  end
  else
  begin
    Padding.Top := 4;
    Height := 32;
  end;
end;

procedure TFrameAttachmentDocument.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

end.

