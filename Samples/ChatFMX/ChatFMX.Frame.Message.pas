unit ChatFMX.Frame.Message;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, VK.Entity.Message, VK.Entity.PushSettings, VK.API,
  VK.Entity.Conversation, FMX.Memo.Style;

type
  TFrameMessage = class(TFrame)
    LayoutLeft: TLayout;
    RectangleBG: TRectangle;
    Layout1: TLayout;
    Circle1: TCircle;
    LayoutRight: TLayout;
    PathSelected: TPath;
    LayoutClient: TLayout;
    LayoutFrom: TLayout;
    LabelFrom: TLabel;
    LabelTime: TLabel;
    MemoText: TMemo;
    procedure MemoTextChange(Sender: TObject);
    procedure MemoTextResize(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure FOnMemoApply(Sender: TObject);
    procedure Circle1Click(Sender: TObject);
  private
    FVK: TCustomVK;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    procedure Fill(Item: TVkMessage; Data: TVkMessageHistory);
  end;

implementation

uses
  System.Math;

{$R *.fmx}

function GetTextRect(Control: TCustomMemo): TRectF;
begin
  if Assigned(Control.Canvas) then
  begin                    //Control.ContentSize.Size.Width
    Result := RectF(0, 0, Control.Width, 10000);
    Control.Canvas.Font.Assign(Control.Font);
    //Control.Canvas.Font.Size := Control.Canvas.Font.Size + 1;
    Control.Canvas.MeasureText(Result,
      Control.Text,
      Control.WordWrap, [],
      Control.TextSettings.HorzAlign,
      Control.TextSettings.VertAlign);
  end
  else
    Result := TRectF.Empty;
end;

procedure TFrameMessage.Circle1Click(Sender: TObject);
begin
  MemoTextChange(nil);
  //ShowMessage(MemoText.Width.ToString);
end;

constructor TFrameMessage.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  Name := '';
  FVK := AVK;
  RectangleBG.Opacity := 0;
  PathSelected.Visible := False;
  MemoText.DisableDisappear := True;
  MemoText.OnApplyStyleLookup := FOnMemoApply;
end;

procedure TFrameMessage.Fill(Item: TVkMessage; Data: TVkMessageHistory);
begin
  MemoText.Text := Item.Text;
  (MemoText.Presentation as TStyledMemo).InvalidateContentSize;

  (MemoText.Presentation as TStyledMemo).PrepareForPaint;
end;

procedure TFrameMessage.FOnMemoApply(Sender: TObject);
begin        {
  TThread.ForceQueue(nil,
    procedure
    begin
      MemoTextChange(Sender);
    end);  }
end;

procedure TFrameMessage.FrameResize(Sender: TObject);
begin
  var Sz: Single := Padding.Top + Padding.Bottom;
  for var Control in LayoutClient.Controls do
    Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  Height := Max(Sz, 60);
  if Assigned(ParentControl) then
    ParentControl.RecalcSize;
end;

procedure TFrameMessage.MemoTextChange(Sender: TObject);
begin
  MemoText.Height := MemoText.ContentSize.Size.Height + 5;
  FrameResize(nil);
  //MemoText.Height := GetTextRect(MemoText).Height;
end;

procedure TFrameMessage.MemoTextResize(Sender: TObject);
begin
  MemoTextChange(Sender);
end;

end.

