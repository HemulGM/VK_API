unit ChatFMX.Frame.Loading;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Objects, FMX.Layouts;

type
  TFrameLoading = class(TFrame)
    Circle1: TCircle;
    Circle3: TCircle;
    Circle2: TCircle;
    ColorAnimationAnimC: TColorAnimation;
    ColorAnimationAnimL: TColorAnimation;
    ColorAnimationAnimR: TColorAnimation;
    LayoutAni: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.

