unit ChatFMX.DM.Res;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList,
  FMX.SVGIconImageList;

type
  TDataModuleRes = class(TDataModule)
    ImageListSVG: TSVGIconImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModuleRes: TDataModuleRes;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
