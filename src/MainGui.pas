unit MainGui;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Styles,
  FMX.Controls.Presentation,
  CastleApp, FMX.Layouts, FMX.StdCtrls, FMX.Menus, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.TabControl, Fmx.CastleControl, System.Rtti,
  FMX.Grid.Style, FMX.Grid;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    LayoutTop: TLayout;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    LayoutView: TLayout;
    LayoutLeft: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    Layout4: TLayout;
    Switch3D: TSwitch;
    Label3D: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LayoutLeftResize(Sender: TObject);
    procedure Switch3DClick(Sender: TObject);
    procedure SwitchView;
    procedure Layout4Resize(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    CastleControl: TCastleControl;
    CastleApp: TCastleApp;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  CastleVectors,
  SpritelySettings;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  with CastleApp do
    begin
      if Models.Kids.Count > 1 then
        begin
          for I := 0 to Models.Kids.Count - 1 do
            Models.Kids[i].Normalize;
          Models.Kids[1].Translation := Models.Kids[1].Translation  + Vector3(1,0,0);
        end;

    end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CastleControl := TCastleControl.Create(LayoutView);
  CastleControl.Align := TAlignLayout.Client;
  CastleControl.Parent := LayoutView;
  CastleApp := TCastleApp.Create(CastleControl);
  CastleControl.Container.View := CastleApp;
  SwitchView;

  StyleBook1.UseStyleManager := True;
  TStyleManager.SetStyleFromFile('Styles/Win10ModernBlue.style');
//  TStyleManager.SetStyleFromFile('Styles/Sterling.Win.style');
end;

procedure TForm1.LayoutLeftResize(Sender: TObject);
begin
  Memo1.Height := LayoutLeft.Height - Button1.Height;
  Memo1.Width := LayoutLeft.Width;
  Button1.Position.X := (Memo1.Width - Button1.Width) / 2;
  Button1.Position.Y := Memo1.Height;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := SystemSettings.LoadFromDir;
  OpenDialog1.Filter:='GLTF Models|*.gltf;*.glb|OBJ Models|*.obj|DAE Models|*.dae|STL Models|*.stl|X3D Models|*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz';
  if OpenDialog1.Execute then
    begin
//      Caption := APPNAME + ': ' + OpenDialog1.FileName;
    end;
end;

procedure TForm1.Layout4Resize(Sender: TObject);
begin
{
  Switch3D.Width := Layout4.Width - 16;
  Switch3D.Position.X := 8;
  Switch3D.Position.Y := 8;
  Label3D.Width := Switch3D.Width;
  Label3D.Position.X := Switch3D.Position.X;
  Label3D.Position.Y := Switch3D.Position.Y + Switch3D.Height + 8;
  }
end;

procedure TForm1.Switch3DClick(Sender: TObject);
begin
  SwitchView;
end;

procedure TForm1.SwitchView;
begin
  CastleApp.SwitchView3D(Switch3D.IsChecked);
end;

end.

