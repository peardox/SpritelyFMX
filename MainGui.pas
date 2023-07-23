unit MainGui;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, fmx.castlecontrol, CastleUIControls, CastleVectors,
  CastleGLUtils, CastleColors, FMX.Layouts;

type
  TCastleApp = class(TCastleView)
    procedure Render; override; // TCastleUserInterface
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    GLWin: TCastleControl;
    GLView: TCastleApp;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLWin := TCastleControl.Create(Self);

  GLWin.Parent := Layout1;
  GLWin.Align := TAlignLayout.Client;
  GLView := TCastleApp.Create(GLWin);
  GLWin.Container.View := GLView;
end;

procedure TCastleApp.Render;
var
  Points: array[0..3] of TVector2;
begin
  inherited;
  Points[0] := Vector2(0, Container.UnscaledHeight / 2);
  Points[1] := Vector2(Container.UnscaledWidth, Container.UnscaledHeight / 2);
  Points[2] := Vector2(Container.UnscaledWidth / 2, 0);
  Points[3] := Vector2(Container.UnscaledWidth / 2, Container.UnscaledHeight);

  DrawPrimitive2D(pmLines, Points, Green);

end;

end.
