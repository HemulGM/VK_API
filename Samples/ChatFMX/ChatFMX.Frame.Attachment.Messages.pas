unit ChatFMX.Frame.Attachment.Messages;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, VK.API, VK.Entity.Doc,
  VK.Types, System.Messaging;

type
  TFrameAttachmentMessages = class(TFrame)
    LabelTitle: TLabel;
    procedure LabelTitleMouseLeave(Sender: TObject);
    procedure LabelTitleMouseEnter(Sender: TObject);
  private
    FVK: TCustomVK;
    FTitle: string;
    procedure SetTitle(const Value: string);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    procedure Fill(const Count: Integer; const RootMessage: Integer; Fwd: Boolean);
    property Title: string read FTitle write SetTitle;
  end;

implementation

uses
  ChatFMX.Utils;

{$R *.fmx}

constructor TFrameAttachmentMessages.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
end;

procedure TFrameAttachmentMessages.Fill(const Count: Integer; const RootMessage: Integer; Fwd: Boolean);
begin
  if Fwd then
    Title := Count.ToString + ' ' + WordOfCount(Count, ['пересланное', 'пересланных', 'пересланных']) + ' ' + WordOfCount(Count, ['сообщение', 'сообщения', 'сообщений'])
  else
    Title := 'в ответ на сообщение';
end;

procedure TFrameAttachmentMessages.LabelTitleMouseEnter(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.Font.Style := Control.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentMessages.LabelTitleMouseLeave(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.Font.Style := Control.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentMessages.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

end.

