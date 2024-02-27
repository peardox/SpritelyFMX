unit MainGui;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Styles,
  FMX.Controls.Presentation,
  CastleApp, FMX.Layouts, FMX.StdCtrls, FMX.Menus, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.TabControl, Fmx.CastleControl, System.Rtti,
  FMX.Grid.Style, FMX.Grid,
  CastleModel,
  SpritelyControls,
  SpritelySettings,
  SpritelyTypes, FMX.Objects, FMX.ListBox
  ;

type
  TAxisButton = record
    AxisName: String;
    Control: TButton;
  end;

  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    LayoutTop: TLayout;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuLoad: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    TabProject: TTabItem;
    TabOutput: TTabItem;
    LayoutView: TLayout;
    LayoutLeft: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    Layout4: TLayout;
    View3D: TLayout;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    TabControl2: TTabControl;
    TabModel: TTabItem;
    TabCamera: TTabItem;
    CameraRotationLayout: TLayout;
    CameraInclinationLayout: TLayout;
    Layout2D3D: TLayout;
    ModelRotationLayout: TLayout;
    Label3D: TLabel;
    Switch3D: TSwitch;
    CheckBox1: TCheckBox;
    StringGrid2: TStringGrid;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    MenuItem3: TMenuItem;
    mnuCheckGLTF: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure LayoutLeftResize(Sender: TObject);
    procedure Switch3DClick(Sender: TObject);
    procedure SwitchView;
    procedure Layout4Resize(Sender: TObject);
    procedure mnuLoadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LayoutViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure LayoutViewResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure TabModelClick(Sender: TObject);
    procedure TabCameraClick(Sender: TObject);
    procedure ModelMove(const OX, OY: Single);
    procedure CheckBox1Change(Sender: TObject);
    procedure mnuCheckGLTFClick(Sender: TObject);
  private
    { Private declarations }
    cameraAngle: TPDXRadialDial;
    cameraInclination: TPDXArcDial;
    modelAngle: TPDXRadialDial;
    CastleControl: TCastleControl;
    CastleApp: TCastleApp;
    OutputSize: TUnitConfig;
    procedure NewModel(Sender: TObject; const AModel: TCastleModel);
    procedure LogTicker(Sender: TObject; const Msg: String);
    procedure RotateModel(Sender: TObject; const ARotation: Single);
    procedure RotateCamera(Sender: TObject; const ARotation: Single);
    procedure InclineCamera(Sender: TObject; const ARotation: Single);
    procedure UpdateModelInfo;
  public
    { Public declarations }
  end;


var
  Form1: TForm1;
  DoneOne: Boolean;

implementation

uses
  Math,
  SpritelyCheckTextures,
  CastleHelpers,
  CastleVectors;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  E: TExtents;
  model: TCastleModel;
begin

  model := CastleApp.SelectedModel;
  if model <> Nil then
  begin
    E:=CastleApp.Viewport.CalcAngles(model);
    Memo1.Lines.Add('Min : ' + E.Min.ToString);
    Memo1.Lines.Add('Max : ' + E.Max.ToString);
    Memo1.Lines.Add('Size: ' + E.Size.ToString);
    Memo1.Lines.Add('Pix : ' + E.Pixels.ToString);
    Memo1.Lines.Add('Stage : ' + CastleApp.Stage.BoundingBox.Size.ToString);
  end;

//  cameraInclination.Angle := 0.81625;
  if not DoneOne then
    begin
//      cameraInclination.Angle := ((Pi/2) - 0.615088935); // True ISO
      cameraInclination.Angle := ((Pi/2) - (pi / 6)); // 2:1 ISO
//      cameraInclination.Angle := ((Pi/2) - 0.955316618); // sqrt(2)
//      cameraInclination.Angle := ((Pi/2) - 1.10714872); // (3/4)
      cameraAngle.Angle := 0.785398185253143;
      DoneOne := True;
    end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
var
  model: TCastleModel;
begin
  model := CastleApp.SelectedModel;
  if model <> Nil then
  begin
    model.ShowDebugBox(CheckBox1.IsChecked);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoneOne := False;
  OutputSize := UnitConfig(256, 256);
  CastleControl := TCastleControl.Create(View3D);
  CastleControl.Align := TAlignLayout.Client;
  CastleControl.Parent := View3D;
  CastleApp := TCastleApp.Create(CastleControl);
  CastleApp.OnExtMessage := LogTicker;
  CastleApp.OnModel := NewModel;
  CastleControl.Container.View := CastleApp;

  StyleBook1.UseStyleManager := True;
//  TStyleManager.SetStyleFromFile('Styles/RubyGraphite.style');
  {$IF DEFINED(MSWINDOWS)}
  TStyleManager.SetStyleFromFile('Styles/Win10Modern.style');
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  TStyleManager.SetStyleFromFile('Styles/MaterialOxfordBlue_Linux.style');
  {$ENDIF}

//  TStyleManager.SetStyleFromFile('Styles/Radiant.Win.style');
//  TStyleManager.SetStyleFromFile('Styles/Vapor.Win.style');
//  TStyleManager.SetStyleFromFile('Styles/Win10ModernBlue.style');
//  TStyleManager.SetStyleFromFile('Styles/Sterling.Win.style');
//  TStyleManager.SetStyleFromFile('Styles/Win10ModernDark.style');
//  TStyleManager.SetStyleFromFile('Styles/MaterialOxfordBlue_Win.style');

  if ReportMemoryLeaksOnShutdown then
    Caption := APPNAME + ' (ReportMemoryLeaksOnShutdown)'
  else
    Caption := APPNAME;

//  CameraInclinationLayout.Height := CameraInclinationLayout.Width;

  modelAngle := TPDXRadialDial.Create(ModelRotationLayout);
  modelAngle.OnExtMessage := LogTicker;
  modelAngle.OnRotate := RotateModel;
  cameraAngle := TPDXRadialDial.Create(CameraRotationLayout);
  cameraAngle.OnRotate := RotateCamera;
  cameraAngle.Steps := 360;
  cameraInclination := TPDXArcDial.Create(CameraInclinationLayout);
  cameraInclination.OnRotate := InclineCamera;
  cameraInclination.Max := Pi;
//  cameraInclination.Angle := Pi / 2;
  SwitchView;
//  camAngle.Position.X := 0;
//  camAngle.Position.Y := 100;
  StringGrid1.RowCount := 11;
  StringGrid1.Cells[0,0] := 'Translation X';
  StringGrid1.Cells[0,1] := 'Translation Y';
  StringGrid1.Cells[0,2] := 'Translation Z';
  StringGrid1.Cells[0,3] := 'Rotation Angle';
  StringGrid1.Cells[0,4] := 'Scale';
  StringGrid1.Cells[0,5] := 'Center X';
  StringGrid1.Cells[0,6] := 'Center Y';
  StringGrid1.Cells[0,7] := 'Center Z';
  StringGrid1.Cells[0,8] := 'Size X';
  StringGrid1.Cells[0,9] := 'Size Y';
  StringGrid1.Cells[0,10] := 'Size Z';
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
//  Memo1.Lines.Add('KeyPress : Key = ' + IntToStr(Key));
  case Key of
    37: begin // Left
          ModelMove( -0.01,  0);
        end;
    38: begin // Up
          ModelMove(  0, +0.01);
        end;
    39: begin // Right
          ModelMove( +0.01,  0);
        end;
    40: begin //Down
          ModelMove(  0, -0.01);
        end;
  end;

end;

procedure TForm1.UpdateModelInfo;
var
  inf: TModelInfo;
  model: TCastleModel;
begin
  model := CastleApp.SelectedModel;
  if model <> Nil then
  begin
    CheckBox1.IsChecked := model.HasDebugBox;

    inf := model.GetInfo;
    StringGrid1.Cells[1,0] := FormatFloat('###0.000', inf.Translation.X);
    StringGrid1.Cells[1,1] := FormatFloat('###0.000', inf.Translation.Y);
    StringGrid1.Cells[1,2] := FormatFloat('###0.000', inf.Translation.Z);
    StringGrid1.Cells[1,3] := FormatFloat('###0.000', inf.Rotation.W);
    StringGrid1.Cells[1,4] := FormatFloat('###0.000', inf.Scale.X);
    StringGrid1.Cells[1,5] := FormatFloat('###0.000', inf.Center.X * inf.Scale.X);
    StringGrid1.Cells[1,6] := FormatFloat('###0.000', inf.Center.Y * inf.Scale.X);
    StringGrid1.Cells[1,7] := FormatFloat('###0.000', inf.Center.Z * inf.Scale.X);
    StringGrid1.Cells[1,8] := FormatFloat('###0.000', inf.Size.X * inf.Scale.X);
    StringGrid1.Cells[1,9] := FormatFloat('###0.000', inf.Size.Y * inf.Scale.X);
    StringGrid1.Cells[1,10] := FormatFloat('###0.000', inf.Size.Z * inf.Scale.X);
  end;

end;

procedure TForm1.LayoutLeftResize(Sender: TObject);
begin
//  Button1.Height := 0;

  Memo1.Height := LayoutLeft.Height - Button1.Height;
  Memo1.Width := LayoutLeft.Width;
  Button1.Position.X := (Memo1.Width - Button1.Width) / 2;
  Button1.Position.Y := Memo1.Height;
end;

procedure TForm1.LayoutViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  factor: Integer;
begin
  factor := 1;

  if WheelDelta < 0 then
    CastleApp.ZoomOut(factor);
  if WheelDelta > 0 then
    CastleApp.ZoomIn(factor);
end;

procedure TForm1.LayoutViewResize(Sender: TObject);
var
  WorkingScale: Single;
begin
  if(OutputSize.Width>0) and (OutputSize.Height > 0) then // Div0 trap - can't happen anyway but safer
    begin
      WorkingScale := Min(LayoutView.Width / OutputSize.Width, LayoutView.Height / OutputSize.Height);
      if WorkingScale <> Infinity then
        begin
          View3D.Width := OutputSize.Width * WorkingScale;
          View3D.Height := OutputSize.Height * WorkingScale;
          View3D.Position.X := (LayoutView.Width - View3D.Width) / 2;
          View3D.Position.Y := (LayoutView.Height - View3D.Height) / 2;
        end;
    end;
end;

procedure TForm1.LogTicker(Sender: TObject; const Msg: String);
begin
//  Memo1.Lines.Clear;
  Memo1.Lines.Add(Msg);
end;

procedure TForm1.mnuLoadClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := SystemSettings.LoadFromDir;
  OpenDialog1.Filter:='GLTF Models|*.gltf;*.glb|OBJ Models|*.obj|DAE Models|*.dae|STL Models|*.stl|X3D Models|*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz';
  if OpenDialog1.Execute then
    begin
      if ReportMemoryLeaksOnShutdown then
        Caption := APPNAME + ': ' + OpenDialog1.FileName + ' (ReportMemoryLeaksOnShutdown)'
      else
        Caption := APPNAME + ': ' + OpenDialog1.FileName;
      CastleApp.AddModel(OpenDialog1.FileName);
      SystemSettings.LastModel := OpenDialog1.FileName;
    end;
end;

procedure TForm1.mnuCheckGLTFClick(Sender: TObject);
var
  SelDir: String;
begin
  if SelectDirectory('Check GLTF models (recursively)', SystemSettings.SearchDir, SelDir) then
    begin
      if ReportMemoryLeaksOnShutdown then
        Caption := APPNAME + ': Scanning ' + SelDir + ' (ReportMemoryLeaksOnShutdown)'
      else
        Caption := APPNAME + ': Scanning ' + SelDir;
//      CastleApp.AddModel(OpenDialog1.FileName);
//      SystemSettings.LastModel := OpenDialog1.FileName;
      CheckGLTFTextures(SelDir);
      SystemSettings.SearchDir := SelDir;

    end;
end;

procedure TForm1.ModelMove(const OX, OY: Single);
var
  model: TCastleModel;
begin
  model := CastleApp.SelectedModel;
  if model <> Nil then
  begin
    model.Translation := model.Translation + Vector3(OX,0,OY);
    UpdateModelInfo;
  end;
end;

procedure TForm1.NewModel(Sender: TObject; const AModel: TCastleModel);
begin
  UpdateModelInfo;
end;

procedure TForm1.InclineCamera(Sender: TObject; const ARotation: Single);
begin
  CastleApp.Inclination := ARotation;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Inc = ' + FloatToStr(ARotation));
  CastleApp.ApplyView;
end;

procedure TForm1.RotateCamera(Sender: TObject; const ARotation: Single);
begin
  CastleApp.Azimuth := ARotation;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Azi = ' + FloatToStr(ARotation));
  CastleApp.ApplyView;
end;

procedure TForm1.RotateModel(Sender: TObject; const ARotation: Single);
var
  model: TCastleModel;
begin
  model := CastleApp.SelectedModel;
  if model <> Nil then
  begin
    model.Gimbal.Rotation := Vector4(0,1,0,ARotation);
    UpdateModelInfo;
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
  if Switch3D.IsChecked then
    begin
      Label3D.Text := '3D';
    end
  else
    begin
      Label3D.Text := '2D';
    end;
  CastleApp.SwitchView3D(Switch3D.IsChecked);
end;

procedure TForm1.TabCameraClick(Sender: TObject);
begin
  CastleControl.SetFocus;
end;

procedure TForm1.TabModelClick(Sender: TObject);
begin
  CastleControl.SetFocus;
end;

end.

