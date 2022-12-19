unit ChatFMX.Frame.Window;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, FMX.Objects, FMX.Ani;

type
  TFrameWindow = class(TFrame)
    RectangleBackground: TRectangle;
    procedure RectangleBackgroundClick(Sender: TObject);
  private
    FVK: TCustomVK;
    procedure SetVK(const Value: TCustomVK);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    procedure ShowFrame;
    procedure HideFrame;
    property VK: TCustomVK read FVK write SetVK;
  end;

implementation

{$R *.fmx}

{ TFrameWindow }

constructor TFrameWindow.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  Name := '';
  Visible := False;
  FVK := AVK;
end;

procedure TFrameWindow.HideFrame;
begin
  TAnimator.AnimateFloatWait(Self, 'Opacity', 0);
  Visible := False;
  TThread.ForceQueue(nil, Free);
end;

procedure TFrameWindow.RectangleBackgroundClick(Sender: TObject);
begin
  HideFrame;
end;

procedure TFrameWindow.SetVK(const Value: TCustomVK);
begin
  FVK := Value;
end;

procedure TFrameWindow.ShowFrame;
begin
  Opacity := 0;
  Visible := True;
  BringToFront;
  TAnimator.AnimateFloat(Self, 'Opacity', 1);
end;

end.

