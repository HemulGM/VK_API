unit ChatFMX.Frame.Attachment.Poll;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatFMX.Frame.Attachment, FMX.Objects, FMX.Layouts, FMX.Controls.Presentation,
  VK.Entity.Poll, System.Messaging, FMX.Ani;

type
  TButton = class(FMX.StdCtrls.TButton)
  public
    procedure SetText(const Value: string); override;
    procedure ApplyStyleLookup; override;
  end;

  TFrameAttachmentPoll = class(TFrameAttachment)
    RectangleBackground: TRectangle;
    RectangleGradient: TRectangle;
    LayoutClient: TLayout;
    LayoutHead: TLayout;
    LayoutText: TLayout;
    LabelSubtitle: TLabel;
    LabelAuthor: TLabel;
    LabelQuestion: TLabel;
    LayoutAnswers: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    LayoutFooter: TLayout;
    LabelVotes: TLabel;
    LayoutOptions: TLayout;
    Path1: TPath;
    ButtonVote: TButton;
    LayoutBG: TLayout;
    RectangleFrame: TRectangle;
    procedure LayoutOptionsClick(Sender: TObject);
    procedure ButtonVoteClick(Sender: TObject);
  private
    FImageUrl: string;
    FImageFile: string;
    FVoted: Boolean;
    FIsMultiple: Boolean;
    FEndDate: TDateTime;
    FAnonymous: Boolean;
    FDisableUnvote: Boolean;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure FOnButtonResize(Sender: TObject);
    procedure SetVoted(const Value: Boolean);
    procedure FOnVoteClick(Sender: TObject);
    procedure SetIsMultiple(const Value: Boolean);
    procedure UpdateButtons;
    procedure UpdateVoted;
    procedure SetAnonymous(const Value: Boolean);
    procedure SetEndDate(const Value: TDateTime);
    procedure UpdateInfo;
    procedure SetDisableUnvote(const Value: Boolean);
  public
    procedure SetVisibility(const Value: Boolean); override;
    procedure Fill(Item: TVkPoll);
    destructor Destroy; override;
    property Voted: Boolean read FVoted write SetVoted;
    property IsMultiple: Boolean read FIsMultiple write SetIsMultiple;
    property Anonymous: Boolean read FAnonymous write SetAnonymous;
    property EndDate: TDateTime read FEndDate write SetEndDate;
    property DisableUnvote: Boolean read FDisableUnvote write SetDisableUnvote;
  end;

var
  FrameAttachmentPoll: TFrameAttachmentPoll;

implementation

uses
  VK.Entity.Common, System.Math, ChatFMX.Utils, VK.Types, ChatFMX.PreviewManager;

{$R *.fmx}

{ TFrameAttachmentPoll }

procedure TFrameAttachmentPoll.ButtonVoteClick(Sender: TObject);
begin
  Voted := True;
end;

destructor TFrameAttachmentPoll.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentPoll.Fill(Item: TVkPoll);
begin
  if Assigned(Item.Photo) then
  begin
    RectangleFrame.Visible := False;
    RectangleGradient.Visible := True;
    RectangleBackground.Align := TAlignLayout.Client;
    RectangleBackground.Fill.Kind := TBrushKind.Solid;
    RectangleBackground.Fill.Color := StrToInt('$FF' + Item.Photo.Color);
    RectangleGradient.Fill.Gradient.Color := StrToInt('$00' + Item.Photo.Color);
    RectangleGradient.Fill.Gradient.Color1 := StrToInt('$FF' + Item.Photo.Color);
    var Size := Item.Photo.Images.GetSizeFromWidth(510);
    if Assigned(Size) then
      FImageUrl := Size.Url;
  end
  else if Assigned(Item.Background) then
  begin
    RectangleFrame.Visible := False;
    RectangleGradient.Visible := False;
    RectangleBackground.Align := TAlignLayout.Client;
    RectangleBackground.Fill.Kind := TBrushKind.Gradient;
    RectangleBackground.Fill.Gradient.Points.Clear;

    for var GrPoint in Item.Background.Points do
    begin
      var Point := TGradientPoint(RectangleBackground.Fill.Gradient.Points.Add);
      Point.Offset := GrPoint.Position;
      Point.Color := StrToInt('$FF' + GrPoint.Color);
    end;
    RectangleBackground.Fill.Gradient.RadialTransform.RotationAngle := 180 + Item.Background.Angle;
  end
  else
  begin
    RectangleBackground.Visible := False;
    RectangleFrame.Visible := True;
    RectangleGradient.Visible := True;
    RectangleGradient.Fill.Kind := TBrushKind.Solid;
    RectangleGradient.Fill.Color := $0AFFFFFF;
  end;
  LabelQuestion.Text := Item.Question;
  LayoutText.Height := LabelQuestion.Height + 17 + 17 + 16;
  LayoutText.Width := LayoutHead.Width - (LayoutHead.Padding.Left + LayoutHead.Padding.Right);
  if Item.Votes > 0 then
    LabelVotes.Text := WordOfCount(Item.Votes, ['Проголосовал', 'Проголосовало', 'Проголосовали']) + ' ' + Item.Votes.ToString + ' ' + WordOfCount(Item.Votes, ['человек', 'человека', 'человек'])
  else
    LabelVotes.Text := 'Проголосуйте ' + WordOfSex(VK.UserSex, ['первым!', 'первой!']);
  while LayoutAnswers.ControlsCount > 0 do
    LayoutAnswers.Controls[0].Free;
  Anonymous := Item.Anonymous;
  EndDate := Item.EndDate;
  DisableUnvote := Item.DisableUnvote;
  for var Answer in Item.Answers do
  begin
    var Button := TButton.Create(LayoutAnswers);
    Button.Parent := LayoutAnswers;
    Button.Position.Y := 1000;
    Button.Height := 30;
    Button.Align := TAlignLayout.Top;
    Button.Margins.Rect := TRectF.Create(0, 5, 0, 5);
    Button.TextSettings.HorzAlign := TTextAlign.Leading;
    Button.Text := Answer.Text;
    Button.StylesData['votes'] := ' ⋅ ' + Answer.Votes.ToString;
    Button.StylesData['rate'] := Answer.Rate.ToString + ' %';
    Button.StylesData['v_rate'] := Answer.Rate;
    var VoteAnswer := Item.AnswerIds.Contains(Answer.Id);
    Button.StylesData['voted.Visible'] := VoteAnswer;

    Button.StylesData['check_vote_ckecked.Visible'] := False;
    Button.StylesData['checked.Visible'] := False;

    Button.OnResize := FOnButtonResize;
    Button.OnClick := FOnVoteClick;
    Button.TextSettings.WordWrap := True;
    Button.StyleLookup := 'button_vote_answer';
    FOnButtonResize(Button);
  end;

  IsMultiple := Item.Multiple;
  Voted := Length(Item.AnswerIds) > 0;
  UpdateVoted;
end;

procedure TFrameAttachmentPoll.FOnButtonResize(Sender: TObject);
var
  Button: TButton absolute Sender;
begin
  Button.StylesData['bg_progress.Width'] := Button.Width * (Button.StylesData['v_rate'].AsExtended / 100);
  var H: Single := 0;
  for var Control in LayoutAnswers.Controls do
    H := H + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  Height := LayoutHead.Height + LayoutFooter.Height + Padding.Top + H;
end;

procedure TFrameAttachmentPoll.FOnVoteClick(Sender: TObject);
var
  Button: TButton absolute Sender;
begin
  if FIsMultiple then
  begin
    Button.StylesData['check_vote_ckecked.Visible'] := not Button.StylesData['check_vote_ckecked.Visible'].AsBoolean;
    Button.StylesData['checked.Visible'] := Button.StylesData['check_vote_ckecked.Visible'].AsBoolean;
    UpdateVoted;
  end
  else
  begin
    Button.StylesData['voted.Visible'] := True;
    Voted := True;
  end;
end;

procedure TFrameAttachmentPoll.UpdateVoted;
begin
  var ExistsVote: Boolean := False;
  if not FVoted then
  begin
    for var Control in LayoutAnswers.Controls do
      if Control is TButton then
      begin
        var Button := Control as TButton;
        if Button.StylesData['checked.Visible'].AsBoolean then
        begin
          ExistsVote := True;
          Break;
        end;
      end;
  end;
  ButtonVote.Visible := ExistsVote;
  LabelVotes.Visible := not ExistsVote;
end;

procedure TFrameAttachmentPoll.LayoutOptionsClick(Sender: TObject);
begin
  Voted := False;
end;

procedure TFrameAttachmentPoll.UpdateInfo;
begin
  var Text := '';
  if Anonymous then
    Text := 'Анонимный опрос'
  else
    Text := 'Публичный опрос';
  if EndDate <> 0 then
    Text := Text + ' · до ' + HumanDateTimeSimple(EndDate, True);
  if DisableUnvote then
    Text := Text + ' · переголосовать нельзя';
  LabelSubtitle.Text := Text;
end;

procedure TFrameAttachmentPoll.SetAnonymous(const Value: Boolean);
begin
  FAnonymous := Value;
  UpdateInfo;
end;

procedure TFrameAttachmentPoll.SetDisableUnvote(const Value: Boolean);
begin
  FDisableUnvote := Value;
  UpdateInfo;
end;

procedure TFrameAttachmentPoll.SetEndDate(const Value: TDateTime);
begin
  FEndDate := Value;
  UpdateInfo;
end;

procedure TFrameAttachmentPoll.SetIsMultiple(const Value: Boolean);
begin
  FIsMultiple := Value;
  UpdateButtons;
end;

procedure TFrameAttachmentPoll.UpdateButtons;
begin
  for var Control in LayoutAnswers.Controls do
    if Control is TButton then
    begin
      var Button := Control as TButton;
      Button.StylesData['votes.Visible'] := FVoted;
      Button.StylesData['state.Visible'] := FVoted;
      Button.StylesData['bg_progress.Visible'] := FVoted;
      Button.StylesData['check_vote_item.Visible'] := FIsMultiple and not FVoted;

      Button.HitTest := not FVoted;
      if FIsMultiple then
      begin
        Button.StylesData['state.Visible'] := FVoted;
      end;
      if FVoted then
      begin
        var Layout: TLayout;
        if Button.FindStyleResource('state', Layout) then
        begin
          Layout.Margins.Right := -150;
          TAnimator.AnimateFloat(Layout, 'Margins.Right', 0, 0.5, TAnimationType.out, TInterpolationType.Sinusoidal);
        end;
        var Rect: TRectangle;
        if Button.FindStyleResource('bg_progress', Rect) then
        begin
          var W := Rect.Width;
          Rect.Width := 0;
          TAnimator.AnimateFloat(Rect, 'Width', W, 1.5 * (Button.StylesData['v_rate'].AsExtended / 100), TAnimationType.out, TInterpolationType.Sinusoidal);
        end;
      end;
    end;
  UpdateVoted;
end;

procedure TFrameAttachmentPoll.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    RectangleBackground.Fill.Bitmap.Bitmap := nil;
end;

procedure TFrameAttachmentPoll.SetVoted(const Value: Boolean);
begin
  FVoted := Value;
  UpdateButtons;
end;

procedure TFrameAttachmentPoll.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
    RectangleBackground.Fill.Bitmap.Bitmap := nil
  else
  try
    RectangleBackground.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    RectangleBackground.Fill.Kind := TBrushKind.Bitmap;
    RectangleBackground.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    RectangleBackground.Align := TAlignLayout.Top;
    RectangleBackground.Height := 200;
  except
    RectangleBackground.Fill.Bitmap.Bitmap := nil;
  end;
end;

{ TButton }

function GetTextRect(Control: TText; MaxWidth: Single): TRectF;
begin
  if Assigned(Control.Canvas) then
  begin
    Result := RectF(0, 0, MaxWidth, 10000);
    Control.Canvas.Font.Assign(Control.Font);
    Control.Canvas.MeasureText(Result, Control.Text, Control.WordWrap, [], TTextAlign.Leading, TTextAlign.Leading);
  end
  else
    Result := TRectF.Empty;
end;

procedure TButton.ApplyStyleLookup;
begin
  inherited;
  var TextItem: TText;
  if FindStyleResource('text', TextItem) then
  begin
    var Rect := GetTextRect(TextItem, 240);
    TextItem.Width := Rect.Width;
    Height := Floor(Max(Rect.Height + 16 + 2, 30));
  end;
end;

procedure TButton.SetText(const Value: string);
begin
  inherited;
  //NeedStyleLookup;
end;

end.

