unit ChatFMX.Frame.Window.OpenLink;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatFMX.Frame.Window, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts;

type
  TFrameWindowLink = class(TFrameWindow)
    LayoutClient: TLayout;
    RectangleBG: TRectangle;
    Rectangle1: TRectangle;
    LayoutFooter: TLayout;
    ButtonCancel: TButton;
    ButtonGo: TButton;
    Layout1: TLayout;
    Label1: TLabel;
    ButtonClose: TButton;
    LabelLink: TLabel;
    procedure ButtonGoClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FLink: string;
  public
    class function Execute(AOwner: TComponent; const Link: string): TFrameWindowLink;
  end;

var
  FrameWindowLink: TFrameWindowLink;

implementation

{$R *.fmx}

procedure TFrameWindowLink.ButtonCancelClick(Sender: TObject);
begin
  HideFrame;
end;

procedure TFrameWindowLink.ButtonCloseClick(Sender: TObject);
begin
  HideFrame;
end;

procedure TFrameWindowLink.ButtonGoClick(Sender: TObject);
begin
  HideFrame;
end;

class function TFrameWindowLink.Execute(AOwner: TComponent; const Link: string): TFrameWindowLink;
begin
  var Form := Application.MainForm;
  Result := TFrameWindowLink.Create(AOwner, nil);
  with Result do
  begin
    FLink := Link;
    LabelLink.Text := FLink;
    Parent := Form;
    Align := TAlignLayout.Contents;
    ShowFrame;
  end;
end;

end.

