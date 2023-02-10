unit ChatFMX.Frame.Keyboard;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, VK.Entity.Keyboard, FMX.Layouts;

type
  TFrameKeyboard = class(TFrame)
    Layout1: TLayout;
  private
    FVK: TCustomVK;
    procedure CreateButtonLine(Items: TVkKeyboardButtons);
    procedure FOnLayoutResize(Sender: TObject);
    procedure FOnKeyButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Item: TVkKeyboard);
  end;

const
  ImgLocation = 'M12,2a8,8,0,0,0-8,8c0,5.34,6.15,12,8,12s8-6.66,8-12A8,8,0,0,0,12,2Zm0,12a4,4,0,1,1,4-4A4,4,0,0,1,12,14Z';
  ImgApp = 'M 10.3 13 h 1.4 a 1.3 1.3 0 0 0 1.3 -1.3 v -1.4 A 1.3 1.3 0 0 0 11.7 9 h -1.4 A 1.3 1.3 0 0 0 9 10.3 v 1.4 a 1.3' +
    ' 1.3 0 0 0 1.3 1.3 Z M 7 11.7 v -1.4 A 1.3 1.3 0 0 0 5.7 9 H 4.3 A 1.3 1.3 0 0 0 3 10.3 v 1.4 A 1.3 1.3 0 0 0 4.3 13 h' +
    ' 1.5 A 1.3 1.3 0 0 0 7 11.7 Z M 4.2 7 h 1.5 A 1.3 1.3 0 0 0 7 5.7 V 4.3 A 1.3 1.3 0 0 0 5.7 3 H 4.3 A 1.3 1.3 0 0 0 3 4.3' +
    ' v 1.5 A 1.3 1.3 0 0 0 4.3 7 Z m 4.9 -1 l 1 1 A 1.3 1.3 0 0 0 12 7 L 13 6 A 1.3 1.3 0 0 0 13 4 l -1 -1 A 1.3 1.3 0 0 0 10 3 l -1 1 A 1.3 1.3 0 0 0 9 6 Z';

implementation

uses
  VK.Types;

{$R *.fmx}

{ TFrameKeyboard }

constructor TFrameKeyboard.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
end;

destructor TFrameKeyboard.Destroy;
begin

  inherited;
end;

procedure TFrameKeyboard.FOnLayoutResize(Sender: TObject);
var
  Layout: TLayout absolute Sender;
begin
  for var Control in Layout.Controls do
    Control.Width := Trunc((Layout.Width / Layout.ControlsCount) - (Control.Margins.Left + Control.Margins.Right));
end;

procedure TFrameKeyboard.FOnKeyButtonClick(Sender: TObject);
begin

end;

procedure TFrameKeyboard.CreateButtonLine(Items: TVkKeyboardButtons);
var
  Layout: TLayout;
begin
  Layout := TLayout.Create(Self);
  Layout.Parent := Self;
  Layout.Position.Y := 10000;
  Layout.Align := TAlignLayout.Top;
  Layout.OnResize := FOnLayoutResize;
  Layout.OnResized := FOnLayoutResize;
  Layout.Height := 48;
  for var Button in Items do
  begin
    if Assigned(Button.Action) then
    begin
      var Btn := TButton.Create(Layout);
      Btn.Position.X := 10000;
      Btn.Align := TAlignLayout.Left;
      Btn.Parent := Layout;
      Btn.Height := 38;
      Btn.Margins.Rect := TRectF.Create(0, 5, 5, 5);
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

      Btn.ApplyStyleLookup;
    end;
  end;
  Layout.RecalcSize;
end;

procedure TFrameKeyboard.Fill(Item: TVkKeyboard);
begin
  for var ButtonLine in Item.Buttons do
    CreateButtonLine(ButtonLine);
  Height := Length(Item.Buttons) * 48;
end;

end.

