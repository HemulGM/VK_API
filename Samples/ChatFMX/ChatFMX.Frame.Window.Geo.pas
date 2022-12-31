unit ChatFMX.Frame.Window.Geo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatFMX.Frame.Window, FMX.Objects, VK.API, FMX.Controls.Presentation,
  FMX.Layouts, ChatFMX.DM.Res, FMX.Edit, FMX.WebBrowser, FMX.Maps,
  System.Sensors;

type
  TFrameWindowGeo = class;

  TExecuteGeoCallBack = reference to procedure(Sender: TFrameWindowGeo; AResult: Boolean);

  TFrameWindowGeo = class(TFrameWindow)
    LayoutContent: TLayout;
    LayoutClient: TLayout;
    RectangleBG: TRectangle;
    LayoutFooter: TLayout;
    Layout1: TLayout;
    Label1: TLabel;
    ButtonClose: TButton;
    ButtonCancel: TButton;
    ButtonSelectGeo: TButton;
    Layout2: TLayout;
    Rectangle1: TRectangle;
    Layout3: TLayout;
    Rectangle2: TRectangle;
    EditSearch: TEdit;
    MapView: TMapView;
    Label2: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSelectGeoClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FCallBack: TExecuteGeoCallBack;
    function GetCoord: TLocationCoord2D;
  public
    class function Execute(AOwner: TComponent; VK: TCustomVK; CallBack: TExecuteGeoCallBack): TFrameWindowGeo;
    property Coord: TLocationCoord2D read GetCoord;
  end;

var
  FrameWindowGeo: TFrameWindowGeo;

implementation

{$R *.fmx}

{ TFrameWindowGeo }

procedure TFrameWindowGeo.ButtonCancelClick(Sender: TObject);
begin
  HideFrame;
end;

procedure TFrameWindowGeo.ButtonCloseClick(Sender: TObject);
begin
  HideFrame;
end;

procedure TFrameWindowGeo.ButtonSelectGeoClick(Sender: TObject);
begin
  if Assigned(FCallBack) then
    FCallBack(Self, True);
  HideFrame;
end;

class function TFrameWindowGeo.Execute(AOwner: TComponent; VK: TCustomVK; CallBack: TExecuteGeoCallBack): TFrameWindowGeo;
begin
  var Form := Application.MainForm;
  Result := TFrameWindowGeo.Create(AOwner, VK);
  with Result do
  begin
    FCallBack := CallBack;
    Parent := Form;
    Align := TAlignLayout.Contents;
    ShowFrame;
  end;
end;

function TFrameWindowGeo.GetCoord: TLocationCoord2D;
begin
  Result := TLocationCoord2D.Create(56.829618, 60.558143);
end;

end.

