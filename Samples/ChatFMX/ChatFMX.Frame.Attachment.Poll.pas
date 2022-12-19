unit ChatFMX.Frame.Attachment.Poll;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatFMX.Frame.Attachment, FMX.Objects, FMX.Layouts, FMX.Controls.Presentation,
  VK.Entity.Poll;

type
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
    Label1: TLabel;
  private
    { Private declarations }
  public
    procedure Fill(Item: TVkPoll);
  end;

var
  FrameAttachmentPoll: TFrameAttachmentPoll;

implementation

{$R *.fmx}

{ TFrameAttachmentPoll }

procedure TFrameAttachmentPoll.Fill(Item: TVkPoll);
begin
  if Assigned(Item.Background) then
  begin
    RectangleGradient.Visible := False;
    RectangleBackground.Fill.Kind := TBrushKind.Gradient;
    RectangleBackground.Fill.Gradient.Points.Clear;
    for var GrPoint in Item.Background.Points do
    begin
      var Point := TGradientPoint(RectangleBackground.Fill.Gradient.Points.Add);
      Point.Offset := GrPoint.Position;
      Point.Color := StrToInt('$FF' + GrPoint.Color);
    end;
    RectangleBackground.Fill.Gradient.RadialTransform.RotationAngle := 180 + Item.Background.Angle;
  end;
  Height := LayoutHead.Height + LayoutFooter.Height + Padding.Top + (40 * Length(Item.Answers));
  LabelQuestion.Text := Item.Question;
  LayoutText.Height := LabelQuestion.Height + 17 + 17 + 16;
  LayoutText.Width := LayoutHead.Width - (LayoutHead.Padding.Left + LayoutHead.Padding.Right);
  while LayoutAnswers.ControlsCount > 0 do
    LayoutAnswers.Controls[0].Free;
  for var Answer in Item.Answers do
  begin
    var Button := TButton.Create(LayoutAnswers);
    Button.Parent := LayoutAnswers;
    Button.Position.Y := 1000;
    Button.Height := 30;
    Button.Align := TAlignLayout.Top;
    Button.Margins.Rect := TRectF.Create(0, 5, 0, 5);
    Button.StyleLookup := 'button_vote_answer';
    Button.TextSettings.HorzAlign := TTextAlign.Leading;
    Button.Text := Answer.Text;
    Button.StylesData['vote'] := Answer.Votes;
    Button.StylesData['rate'] := Answer.Rate;
  end;
end;

end.

