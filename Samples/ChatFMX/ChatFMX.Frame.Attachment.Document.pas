unit ChatFMX.Frame.Attachment.Document;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, VK.API, VK.Entity.Doc,
  VK.Types, System.Messaging, Skia, Skia.FMX, ChatFMX.Frame.Attachment;

type
  TFrameAttachmentDocument = class(TFrameAttachment)
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
    AnimatedImageGIF: TSkAnimatedImage;
    AniIndicatorGif: TAniIndicator;
    procedure LabelTitleMouseLeave(Sender: TObject);
    procedure LabelTitleMouseEnter(Sender: TObject);
    procedure LabelTypeSizeResize(Sender: TObject);
    procedure ImagePreviewClick(Sender: TObject);
    procedure AnimatedImageGIFClick(Sender: TObject);
  private
    FTitle: string;
    FDocSize: string;
    FDocType: TVkDocumentType;
    FModePreview: Boolean;
    FImageUrl: string;
    FImageFile: string;
    FImageUrlGif: string;
    FImageFileGif: string;
    FExt: string;
    FPreviewSize: TSizeF;
    FUrl: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure FOnReadyImageGif(const Sender: TObject; const M: TMessage);
    procedure SetTitle(const Value: string);
    procedure SetDocSize(const Value: string);
    procedure SetDocType(const Value: TVkDocumentType);
    procedure SetModePreview(const Value: Boolean);
    procedure SetExt(const Value: string);
    procedure UpdatePreviewSize;
    procedure SetPreviewSize(const Value: TSizeF);
    procedure SetUrl(const Value: string);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Document: TVkDocument; AsPreview: Boolean);
    property Title: string read FTitle write SetTitle;
    property DocSize: string read FDocSize write SetDocSize;
    property DocType: TVkDocumentType read FDocType write SetDocType;
    property ModePreview: Boolean read FModePreview write SetModePreview;
    property Ext: string read FExt write SetExt;
    property PreviewSize: TSizeF read FPreviewSize write SetPreviewSize;
    property Url: string read FUrl write SetUrl;
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
  ChatFMX.PreviewManager, HGM.FMX.Image, VK.Entity.Common,
  ChatFMX.Frame.Window.Photo;

{$R *.fmx}

procedure TFrameAttachmentDocument.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
  begin
    AnimatedImageGIF.Visible := False;
    ImagePreview.Visible := True;
    AnimatedImageGIF.Source.Data := [];
    if (not FImageUrl.IsEmpty) and (ImagePreview.Bitmap.IsEmpty) then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
  begin
    AnimatedImageGIF.Source.Data := [];
    ImagePreview.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentDocument.AnimatedImageGIFClick(Sender: TObject);
begin
  AnimatedImageGIF.Visible := False;
  ImagePreview.Visible := True;
end;

constructor TFrameAttachmentDocument.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  ImageIcon.Bitmap.LoadFromResource('doc_icons');
  ImageIcon.BitmapMargins.Rect := IconAudio;
  ModePreview := False;
end;

destructor TFrameAttachmentDocument.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImageGif);
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentDocument.Fill(Document: TVkDocument; AsPreview: Boolean);
begin
  Height := 32;
  ModePreview := AsPreview;
  FUrl := Document.Url;
  FImageUrl := '';
  DocSize := Document.SizeStr;
  if Document.Ext.ToLower = 'gif' then
    FImageUrlGif := Document.Url;
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
        //if not FImageUrl.IsEmpty then
        //  TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
      end;
    end;
  end
  else
  begin
    Title := Document.Title;
    DocType := Document.&Type;
  end;
  PreviewSize := TSizeF.Create(Width, Height);
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
    ImagePreview.Bitmap := nil;
  end
  else
  try
    ImagePreview.Bitmap.LoadFromFile(FImageFile);
  except
    ImagePreview.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentDocument.FOnReadyImageGif(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrlGif then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImageGif);
  FImageFileGif := Data.Value.FileName;
  if FImageFileGif.IsEmpty then
  begin
    AnimatedImageGIF.Visible := False;
    ImagePreview.Visible := True;
  end
  else
  try
    AnimatedImageGIF.LoadFromFile(FImageFileGif);
    ImagePreview.Visible := False;
  except
    AnimatedImageGIF.Visible := False;
    ImagePreview.Visible := True;
  end;
  AniIndicatorGif.Visible := False;
end;

procedure TFrameAttachmentDocument.ImagePreviewClick(Sender: TObject);
begin
  if Ext.ToLower = 'gif' then
  begin
    if not FImageUrlGif.IsEmpty then
      if AnimatedImageGIF.Visible then
      begin
        AnimatedImageGIF.Visible := False;
        ImagePreview.Visible := True;
      end
      else
      begin
        ImagePreview.Visible := True;
        AnimatedImageGIF.Visible := True;
        AniIndicatorGif.Visible := True;
        TPreview.Instance.Subscribe(FImageUrlGif, FOnReadyImageGif);
      end;
  end
  else if (Ext.ToLower = 'jpg') or (Ext.ToLower = 'png') or (Ext.ToLower = 'jpeg') then
  begin
    var Form := Application.MainForm;
    with TFrameWindowPhoto.Create(Form, VK) do
    begin
      Parent := Form;
      Align := TAlignLayout.Contents;

      FillUrl(Url);
      ShowFrame;
    end;
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

procedure TFrameAttachmentDocument.SetPreviewSize(const Value: TSizeF);
begin
  FPreviewSize := Value;
end;

procedure TFrameAttachmentDocument.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

procedure TFrameAttachmentDocument.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

end.

