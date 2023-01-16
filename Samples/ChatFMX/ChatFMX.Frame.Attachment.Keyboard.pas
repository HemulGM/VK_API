unit ChatFMX.Frame.Attachment.Keyboard;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatFMX.Frame.Attachment, FMX.Layouts, FMX.Controls.Presentation,
  VK.Entity.Keyboard, VK.Types, FMX.Objects, ChatFMX.Frame.Window.Geo,
  ChatFMX.Frame.Window.OpenLink;

type
  TButton = class(FMX.StdCtrls.TButton)
  public
    procedure ApplyStyleLookup; override;
  end;

  TFrameAttachmentKeyboard = class(TFrameAttachment)
    FlowLayoutButtons: TFlowLayout;
    Button1: TButton;
    Button2: TButton;
    FlowLayoutBreak1: TFlowLayoutBreak;
    Button4: TButton;
    Button3: TButton;
    procedure FlowLayoutButtonsResize(Sender: TObject);
  private
    FPeerId: TVkPeerId;
    procedure FOnKeyButtonClick(Sender: TObject);
    procedure SetPeerId(const Value: TVkPeerId);
    procedure SelectGeo(CallBack: TExecuteGeoCallBack);
    procedure OpenLink(const Link: string);
  public
    procedure Fill(Item: TVkKeyboard);
    property PeerId: TVkPeerId read FPeerId write SetPeerId;
  end;

var
  FrameAttachmentKeyboard: TFrameAttachmentKeyboard;

const
  ImgLocation = 'M12,2a8,8,0,0,0-8,8c0,5.34,6.15,12,8,12s8-6.66,8-12A8,8,0,0,0,12,2Zm0,12a4,4,0,1,1,4-4A4,4,0,0,1,12,14Z';
  ImgApp = 'M 10.3 13 h 1.4 a 1.3 1.3 0 0 0 1.3 -1.3 v -1.4 A 1.3 1.3 0 0 0 11.7 9 h -1.4 A 1.3 1.3 0 0 0 9 10.3 v 1.4 a 1.3' +
    ' 1.3 0 0 0 1.3 1.3 Z M 7 11.7 v -1.4 A 1.3 1.3 0 0 0 5.7 9 H 4.3 A 1.3 1.3 0 0 0 3 10.3 v 1.4 A 1.3 1.3 0 0 0 4.3 13 h' +
    ' 1.5 A 1.3 1.3 0 0 0 7 11.7 Z M 4.2 7 h 1.5 A 1.3 1.3 0 0 0 7 5.7 V 4.3 A 1.3 1.3 0 0 0 5.7 3 H 4.3 A 1.3 1.3 0 0 0 3 4.3' +
    ' v 1.5 A 1.3 1.3 0 0 0 4.3 7 Z m 4.9 -1 l 1 1 A 1.3 1.3 0 0 0 12 7 L 13 6 A 1.3 1.3 0 0 0 13 4 l -1 -1 A 1.3 1.3 0 0 0 10 3 l -1 1 A 1.3 1.3 0 0 0 9 6 Z';

implementation

uses
  System.Math, System.Sensors;

{$R *.fmx}

{ TFrameAttachmentKeyboard }

function GetTextRect(Control: TButton; MaxWidth: Single): TRectF;
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

procedure TFrameAttachmentKeyboard.Fill(Item: TVkKeyboard);
begin
  FlowLayoutButtons.BeginUpdate;
  try
    while FlowLayoutButtons.ControlsCount > 0 do
      FlowLayoutButtons.Controls[0].Free;
  finally
    FlowLayoutButtons.EndUpdate;
  end;
  for var ButtonLine in Item.Buttons do
  begin
    for var Button in ButtonLine do
    begin
      if Assigned(Button.Action) then
      begin
        var Btn := TButton.Create(FlowLayoutButtons);
        Btn.Parent := FlowLayoutButtons;
        Btn.Height := 26;
        Btn.Tag := Ord(Button.Action.&Type);
        Btn.StyleLookup := 'Button_secondary';
        Btn.OnClick := FOnKeyButtonClick;
        Btn.TagString := Button.Action.Payload;
        Btn.Text := Button.Action.&Label;
        Btn.StylesData['app_id'] := Button.Action.AppId;
        Btn.StylesData['owner_id'] := Button.Action.OwnerId;
        Btn.StylesData['hash'] := Button.Action.Hash;
        Btn.StylesData['link'] := Button.Action.Link;
        Btn.StylesData['payload'] := Button.Action.Payload;

        case Button.Action.&Type of
          TVkKeyboardActionType.Text, TVkKeyboardActionType.Callback:
            begin
              case Button.Color of
                TVkKeyboardButtonColor.Positive:
                  Btn.StyleLookup := 'Button_positive';
                TVkKeyboardButtonColor.Negative:
                  Btn.StyleLookup := 'Button_negative';
                TVkKeyboardButtonColor.Primary:
                  Btn.StyleLookup := 'Button_primary';
              else
                Btn.StyleLookup := 'Button_secondary';
              end;
            end;
          TVkKeyboardActionType.VKPay:
            begin
              Btn.Text := 'Оплатить через VK pay';
            end;
          TVkKeyboardActionType.Location:
            begin
              Btn.Text := 'Отправить своё местоположение';
              Btn.StyleLookup := 'Button_img';
              Btn.StylesData['image.Data.Data'] := ImgLocation;
            end;
          TVkKeyboardActionType.OpenApp:
            begin
              Btn.StyleLookup := 'Button_img';
              Btn.StylesData['image.Data.Data'] := ImgApp;
            end;
          TVkKeyboardActionType.OpenLink:
            begin
              Btn.StyleLookup := 'Button_link';
            end;
        end;
      end;
    end;
    var BL := TFlowLayoutBreak.Create(FlowLayoutButtons);
    BL.Parent := FlowLayoutButtons;
  end;
  FlowLayoutButtonsResize(nil);
end;

{ TButton }

procedure TButton.ApplyStyleLookup;
begin
  inherited;
  var ImgW := StylesData['image.Width'].AsExtended;
  if ImgW > 0 then
    ImgW := ImgW + 16;
  Width := GetTextRect(Self, 400).Width + 24 + ImgW;
end;

procedure TFrameAttachmentKeyboard.FlowLayoutButtonsResize(Sender: TObject);
begin
  var H: Single := 0;
  for var Control in FlowLayoutButtons.Controls do
    if not (Control is TFlowLayoutBreak) then
      H := Max(Control.Position.Y + Control.Height, H);

  Height := H + Padding.Top + Padding.Bottom;
end;

procedure TFrameAttachmentKeyboard.FOnKeyButtonClick(Sender: TObject);
var
  Button: TButton absolute Sender;
begin
  case TVkKeyboardActionType(Button.Tag) of
    TVkKeyboardActionType.Text:
      VK.Messages.New.PeerId(PeerId).Payload(Button.TagString).Message(Button.Text).Send;
    TVkKeyboardActionType.OpenLink:
      begin
        OpenLink(Button.StylesData['link'].AsString);
      end;
    TVkKeyboardActionType.Location:
      begin
        SelectGeo(
          procedure(Sender: TFrameWindowGeo; AResult: Boolean)
          begin
            if AResult then
            begin
              var Coord := Sender.Coord;
              VK.Messages.New.PeerId(PeerId).LatLong(Coord.Latitude, Coord.Longitude).Payload(Button.TagString).Send;
            end;
          end);
      end;
  end;
end;

procedure TFrameAttachmentKeyboard.SelectGeo(CallBack: TExecuteGeoCallBack);
begin
  TFrameWindowGeo.Execute(Self, VK, CallBack);
end;

procedure TFrameAttachmentKeyboard.OpenLink(const Link: string);
begin
  TFrameWindowLink.Execute(Self, Link);
end;

procedure TFrameAttachmentKeyboard.SetPeerId(const Value: TVkPeerId);
begin
  FPeerId := Value;
end;

end.

