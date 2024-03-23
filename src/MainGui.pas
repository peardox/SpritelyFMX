unit MainGui;

{$define ClearOnScan}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Styles,
  FMX.Controls.Presentation, FMX.StdActns,
  System.Generics.Collections,
  CastleApp, FMX.Layouts, FMX.StdCtrls, FMX.Menus, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.TabControl, Fmx.CastleControl, System.Rtti,
  FMX.Grid.Style, FMX.Grid,
  CastleModel,
  SpritelyControls,
  SpritelyCheckTextures,
  SpritelySettings,
  FrameToImage,
  SpritelyTypes, FMX.Objects, FMX.ListBox, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.TreeView, System.ImageList, FMX.ImgList
  ;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    LayoutTop: TLayout;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuLoad: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    TabProject: TTabItem;
    TabOutput: TTabItem;
    LayoutView: TLayout;
    LayoutLeft: TLayout;
    Memo1: TMemo;
    Layout4: TLayout;
    View3D: TLayout;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    TabControl2: TTabControl;
    TabModel: TTabItem;
    Layout2D3D: TLayout;
    Label3D: TLabel;
    Switch3D: TSwitch;
    mnuUtilities: TMenuItem;
    mnuCheckGLTF: TMenuItem;
    mnuClear: TMenuItem;
    cbxProjections: TComboBox;
    mnuDirectory: TMenuItem;
    mnuGLTFDir: TMenuItem;
    mnuOBJDir: TMenuItem;
    TreeView1: TTreeView;
    Timer1: TTimer;
    mnuSaveImage: TMenuItem;
    ImageList1: TImageList;
    TabCamera: TTabItem;
    CameraRotationLayout: TLayout;
    CameraInclinationLayout: TLayout;
    StringGrid2: TStringGrid;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    LayoutPreview: TLayout;
    mnuCheckMem: TMenuItem;
    Button1: TButton;
    Label1: TLabel;
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
    procedure ModelMove(const OX, OY, OZ: Single);
    procedure mnuCheckGLTFClick(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure cbxProjectionsChange(Sender: TObject);
    procedure mnuGLTFDirClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure mnuSaveImageClick(Sender: TObject);
    procedure mnuCheckMemClick(Sender: TObject);
  private
    { Private declarations }
    cameraAngle: TPDXRadialDial;
    cameraInclination: TPDXArcDial;
    CastleControl: TCastleControl;
    CastleApp: TCastleApp;
    OutputSize: TUnitConfig;
    TheFileList: TObjectList<TFileDirectory>;
    procedure NewModel(Sender: TObject; const AModel: TCastleModel);
    procedure LogTicker(Sender: TObject; const Msg: String);
    procedure RotateModel(Sender: TObject; const ARotation: Single);
    procedure RotateCamera(Sender: TObject; const ARotation: Single);
    procedure InclineCamera(Sender: TObject; const ARotation: Single);
    procedure UpdateModelInfo;
    procedure UpdateCamInfo;
    procedure CastleClick(Sender: TObject);
    procedure CameraMove(const OX, OY: Single);
    procedure LoadStyle(const AStyle: String = '');
    procedure PopulateProjections;
    procedure TreeViewItemClick(Sender: TObject);
    {$IF defined(MSWINDOWS)}
    procedure MemStat;
    {$ENDIF}
  public
    { Public declarations }
  end;

  {$IF defined(MSWINDOWS)}
  function GetMemoryUsed: UInt64;
  {$ENDIF}

var
  Form1: TForm1;

implementation

uses
  {$IF defined(MSWINDOWS)}
  FastMM4,
  Windows,
  {$ENDIF}
  Math,
  LoadingForm,
  System.DateUtils,
  System.IOUtils,
  CastleLog,
  CastleHelpers,
  CastleCameras,
  CastleTransform,
  CastleVectors;

{$R *.fmx}

{$IF defined(MSWINDOWS)}
{$WARN SYMBOL_PLATFORM OFF}
function GetMemoryUsed: UInt64;
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize
           + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do begin
    result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
  end;
end;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

procedure TForm1.CastleClick(Sender: TObject);
var
  model: TCastleTransform;
begin
  Memo1.Lines.Clear;
  if Assigned(CastleApp) and (CastleApp.Models.Kids.Count > 0) then
    begin
      model := CastleApp.UnderMouse;
      if model is TCastleModel then
        begin
          with model as TCastleModel do
            begin
            SelectModel;
            if HasDebugBox then
              ShowDebugBox(False)
            else
              ShowDebugBox(True);
            end;
        end;
    end;
end;

procedure TForm1.cbxProjectionsChange(Sender: TObject);
begin
  if Assigned(CastleApp) then
    begin
      if (cbxProjections.ItemIndex >= 0) and (cbxProjections.ItemIndex < Length(SystemSettings.Projections)) then
        begin
          cameraInclination.Angle := SystemSettings.Projections[cbxProjections.ItemIndex].Inclination;
          cameraAngle.Angle := SystemSettings.Projections[cbxProjections.ItemIndex].Azimuth;
        end;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Assigned(CastleApp) and CastleApp.IsReady then
    begin
      CastleApp.FitViewToModel(CastleApp.Stage);
      UpdateModelInfo;
    end;
end;

procedure TForm1.PopulateProjections;
var
  Item: TListBoxItem;
  I: Integer;
begin
  for I := 0 to Length(SystemSettings.Projections) - 1 do
    begin
      Item := TListBoxItem.Create(cbxProjections);
      Item.Parent := cbxProjections;
      Item.Text := SystemSettings.Projections[I].Name; // set projection
    end;

  if Length(SystemSettings.Projections) > 4 then
    cbxProjections.ItemIndex := 4;
end;


procedure TForm1.LoadStyle(const AStyle: String = '');
var
  NewStyle: String;
begin
    NewStyle := '';
    { Default to nothing }
  if AStyle = EmptyStr then
    begin
     {$IF DEFINED(MSWINDOWS)}
//      NewStyle := '../../Styles/RubyGraphite.style';
//      NewStyle := '../../Styles/Radiant.Win.style';
//      NewStyle := '../../Styles/Vapor.Win.style';
//      NewStyle := '../../Styles/Win10ModernBlue.style';
      NewStyle := '../../Styles/Sterling.Win.style';
//      NewStyle := '../../Styles/MaterialOxfordBlue_Win.style';
//      NewStyle := '../../Styles/Win10ModernDark.style';
      {$ENDIF}
      {$IF DEFINED(LINUX)}
      NewStyle := 'Styles/MaterialOxfordBlue_Linux.style';
      {$ENDIF}
    end
  else
    NewStyle := AStyle;

  if not(NewStyle = EmptyStr) and FileExists(NewStyle) then
    begin
      StyleBook1.UseStyleManager := True;
      TStyleManager.SetStyleFromFile(NewStyle);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutputSize := UnitConfig(256, 256);
  CastleControl := TCastleControl.Create(View3D);
  CastleControl.Align := TAlignLayout.Client;
  CastleControl.Parent := View3D;
  CastleControl.OnClick := CastleClick;
  CastleApp := TCastleApp.Create(CastleControl);
  CastleApp.OnExtMessage := LogTicker;
  CastleApp.OnModel := NewModel;
  CastleControl.Container.View := CastleApp;

  LoadStyle;

  if ReportMemoryLeaksOnShutdown then
    Caption := APPNAME + ' (ReportMemoryLeaksOnShutdown)'
  else
    Caption := APPNAME;

//  CameraInclinationLayout.Height := CameraInclinationLayout.Width;

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
  StringGrid1.RowCount := 14;
  StringGrid1.Cells[0,0] := 'Extents X';
  StringGrid1.Cells[0,1] := 'Extents Y';
  StringGrid1.Cells[0,2] := 'Viewport';
{
  StringGrid1.Cells[0,0] := 'Translation X';
  StringGrid1.Cells[0,1] := 'Translation Y';
  StringGrid1.Cells[0,2] := 'Translation Z';
}
  StringGrid1.Cells[0,3] := 'Rotation Angle';
  StringGrid1.Cells[0,4] := 'Scale';
  StringGrid1.Cells[0,5] := 'Center X';
  StringGrid1.Cells[0,6] := 'Center Y';
  StringGrid1.Cells[0,7] := 'Center Z';
  StringGrid1.Cells[0,8] := 'Size X';
  StringGrid1.Cells[0,9] := 'Size Y';
  StringGrid1.Cells[0,10] := 'Size Z';
  StringGrid1.Cells[0,11] := 'Name';
  StringGrid1.Cells[0,12] := 'Zoom';
  StringGrid1.Cells[0,13] := 'FOV';

  StringGrid2.RowCount := 14;
  StringGrid2.Cells[0,0] := 'Stage X';
  StringGrid2.Cells[0,1] := 'Stage Y';
  StringGrid2.Cells[0,2] := 'Stage Z';
  StringGrid2.Cells[0,3] := 'Direction X';
  StringGrid2.Cells[0,4] := 'Direction Y';
  StringGrid2.Cells[0,5] := 'Direction Z';
  StringGrid2.Cells[0,6] := 'Center X';
  StringGrid2.Cells[0,7] := 'Center Y';
  StringGrid2.Cells[0,8] := 'Center Z';
  StringGrid2.Cells[0,9] := 'Translation X';
  StringGrid2.Cells[0,10] := 'Translation Y';
  StringGrid2.Cells[0,11] := 'Translation Z';
  StringGrid2.Cells[0,12] := 'Azimuth';
  StringGrid2.Cells[0,13] := 'Inclination';


  UpdateModelInfo;
  UpdateCamInfo;

  PopulateProjections;

  Timer1.Enabled := True;
  Timer1.Interval := 100;
  TheFileList := TObjectList<TFileDirectory>.Create;

  LayoutViewResize(Self);
  {$IF defined(MSWINDOWS)}
  WriteLnLog(Format('Memory Used = %d',[GetMemoryUsed]));
  {$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(TheFileList) then
    TheFileList.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
if TabControl2.ActiveTab = TabModel then
  begin
    case Key of
      37: begin // Left
            ModelMove( -0.01,     0,     0);
          end;
      38: begin // Up
            ModelMove(     0, -0.01,     0);
          end;
      39: begin // Right
            ModelMove( +0.01,     0,     0);
          end;
      40: begin //Down
            ModelMove(     0, +0.01,     0);
          end;
      33: begin // PgUp
            ModelMove(     0,     0, +0.01);
          end;
      34: begin // PgDn
            ModelMove(     0,     0, -0.01);
          end;
      else
        WriteLnLog('Key = ' + IntToStr(Key));
    end;
  end else if TabControl2.ActiveTab = TabCamera then
  begin
    case Key of
      37: begin // Left
            CameraMove( +0.01,  0);
          end;
      38: begin // Up
            CameraMove(  0, -0.01);
          end;
      39: begin // Right
            CameraMove( -0.01,  0);
          end;
      40: begin //Down
            CameraMove(  0, +0.01);
          end;
    end;
  end;

  UpdateModelInfo;
  UpdateCamInfo;

end;

procedure TForm1.UpdateModelInfo;
var
  inf: TModelInfo;
  model: TCastleModel;
  V: TViewStats;
begin
  model := CastleApp.SelectedModel;
  if model <> Nil then
  begin
    if CastleApp.IsReady then
      begin
//        CastleApp.Stage.Center := Vector3(0,0,0);
        V := CastleApp.GetAxis(model);
        StringGrid1.Cells[1,0] := FormatFloat('###0.000', V.Box.View2D.Width);
        StringGrid1.Cells[1,1] := FormatFloat('###0.000', V.Box.View2D.Height);
        StringGrid1.Cells[1,2] := Format('%-4.0f x %-4.0f', [CastleControl.Width, CastleControl.Height]);
        {
        StringGrid1.Cells[1,5] := FormatFloat('###0.000', (V.Box.View2D.Width / 2) - V.Box.View2D.Left);
        StringGrid1.Cells[1,6] := FormatFloat('###0.000', (V.Box.View2D.Height / 2) - V.Box.View2D.Top);
        StringGrid1.Cells[1,7] := '';
        }
      end;

    inf := model.ModelInfo;
    if Assigned(inf) then
      begin
        StringGrid1.Cells[1,3] := FormatFloat('###0.000', inf.Rotation.W);
        StringGrid1.Cells[1,4] := FormatFloat('###0.000', inf.Scale.X);

        StringGrid1.Cells[1,5] := FormatFloat('###0.000', inf.Center.X * inf.Scale.X);
        StringGrid1.Cells[1,6] := FormatFloat('###0.000', inf.Center.Y * inf.Scale.X);
        StringGrid1.Cells[1,7] := FormatFloat('###0.000', inf.Center.Z * inf.Scale.X);

        StringGrid1.Cells[1,8] := FormatFloat('###0.000', inf.Size.X * inf.Scale.X);
        StringGrid1.Cells[1,9] := FormatFloat('###0.000', inf.Size.Y * inf.Scale.X);
        StringGrid1.Cells[1,10] := FormatFloat('###0.000', inf.Size.Z * inf.Scale.X);
        StringGrid1.Cells[1,11] := TPath.GetFileNameWithoutExtension(inf.Name);
      end;

    StringGrid1.Cells[1,12] := FormatFloat('###0.000', CastleApp.Zoom);
    StringGrid1.Cells[1,13] := FormatFloat('###0.00', RadToDeg(CastleApp.FieldOfView));
  end;

end;

procedure TForm1.UpdateCamInfo;
var
  cam: TCastleCamera;
begin
  if Assigned(CastleApp) then
  begin
    cam := CastleApp.Camera.Camera;
//    CastleApp.ApplyView;
    if Assigned(cam) then
      begin
        if not CastleApp.Stage.BoundingBox.IsEmptyOrZero then
          begin
            StringGrid2.Cells[1,0] := FormatFloat('###0.000', CastleApp.Stage.BoundingBox.SizeX);
            StringGrid2.Cells[1,1] := FormatFloat('###0.000', CastleApp.Stage.BoundingBox.SizeY);
            StringGrid2.Cells[1,2] := FormatFloat('###0.000', CastleApp.Stage.BoundingBox.SizeZ);
            StringGrid2.Cells[1,3] := FormatFloat('###0.000', CastleApp.Camera.Direction.X);
            StringGrid2.Cells[1,4] := FormatFloat('###0.000', CastleApp.Camera.Direction.Y);
            StringGrid2.Cells[1,5] := FormatFloat('###0.000', CastleApp.Camera.Direction.Z);
            StringGrid2.Cells[1,6] := FormatFloat('###0.000', CastleApp.Camera.Center.X);
            StringGrid2.Cells[1,7] := FormatFloat('###0.000', CastleApp.Camera.Center.Y);
            StringGrid2.Cells[1,8] := FormatFloat('###0.000', CastleApp.Camera.Center.Z);
            StringGrid2.Cells[1,9] := FormatFloat('###0.000', CastleApp.Camera.Translation.X);
            StringGrid2.Cells[1,10] := FormatFloat('###0.000', CastleApp.Camera.Translation.Y);
            StringGrid2.Cells[1,11] := FormatFloat('###0.000', CastleApp.Camera.Translation.Z);
          end;
        StringGrid2.Cells[1,12] := FormatFloat('###0.000', CastleApp.Azimuth);
        StringGrid2.Cells[1,13] := FormatFloat('###0.000', CastleApp.Inclination);
      end;
  end;
end;

procedure TForm1.LayoutLeftResize(Sender: TObject);
begin
  Memo1.Height := 64;
  Memo1.Width := LayoutLeft.Width;
  LayoutPreview.Width := LayoutLeft.Width;
  LayoutPreview.Height := 240;
  TreeView1.Width := LayoutLeft.Width;
  TreeView1.Height := LayoutLeft.Height - LayoutPreview.Height;
end;

procedure TForm1.LayoutViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  factor: Integer;
begin
  factor := 1;
  if WheelDelta > 0 then
    CastleApp.ZoomOut(factor);
  if WheelDelta < 0 then
    CastleApp.ZoomIn(factor);
  UpdateModelInfo;
  UpdateCamInfo;
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

procedure TForm1.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  fn: String;
  f: TFileDirectory;
begin
  if (AItem.Tag >= 0) and (AItem.Tag < TheFileList.Count) then
    begin
      f := TheFileList.Items[AItem.Tag];
      fn := f.GetFullFileName;
      if FileExists(fn) then
        begin
          if ReportMemoryLeaksOnShutdown then
            Caption := APPNAME + ': ' + fn + ' (ReportMemoryLeaksOnShutdown)'
          else
            Caption := APPNAME + ': ' + fn;
          CastleApp.AddModel(fn);
        end;
    end;
end;

procedure TForm1.LogTicker(Sender: TObject; const Msg: String);
begin
//  Memo1.Lines.Clear;
  Memo1.Lines.Add(Msg);
end;

procedure TForm1.mnuGLTFDirClick(Sender: TObject);
var
  LoadingForm: TfrmLoadingDialog;
  SelDir: String;
  A, I: Integer;
  TItem: TTreeViewItem;
  AItem: TTreeViewItem;
  GItem: TTreeViewItem;
  Added: Integer;
  Shortname: String;
  mi: TModelInfo;
  Start: TDateTime;
  Elapsed: int64;
  AList: TObjectList<TFileDirectory>;
begin
  Added := 0;
  if SelectDirectory('Check GLTF models (recursively)', SystemSettings.SearchDir, SelDir) then
    begin
      if ReportMemoryLeaksOnShutdown then
        Caption := APPNAME + ': Scanning ' + SelDir + ' (ReportMemoryLeaksOnShutdown)'
      else
        Caption := APPNAME + ': Scanning ' + SelDir;
      Start := Now;
      AList := ScanGLTFModels(SelDir);
      try
        if AList.Count > 0 then
          begin
            TreeView1.CollapseAll;
            TreeView1.BeginUpdate;
            LoadingForm := TfrmLoadingDialog.Create(Self);
            try
              Application.ProcessMessages;
              LoadingForm.Show();
              GItem := TTreeViewItem.Create(Self);
              GItem.Text := TPath.GetFileName(AList.Items[0].ParentDir);
              for I := 0 to AList.Count - 1 do
                begin
                  mi := SummariseModel(AList.Items[I].GetFullFileName);
                  LoadingForm.AddingFile(AList.Items[I].GetFullFileName);
                  AList.Items[I].ModelInfo := mi;
                  TItem := TTreeViewItem.Create(Self);
                  TItem.TagObject := AList.Items[I];
                  TItem.TagString := '';
                  if mi.AnimationCount > 0 then
                    begin
                      TItem.ImageIndex := 1;
                      for A := 0 to mi.AnimationCount - 1 do
                        begin
                          AItem := TTreeViewItem.Create(Self);
                          AItem.TagObject := AList.Items[I];
                          AItem.ImageIndex := 2;
                          AItem.Text := mi.Animations[A];
                          AItem.TagString := mi.Animations[A];
                          AItem.Parent := TItem;
                          AItem.OnClick := TreeViewItemClick;
                        end;
                    end
                  else
                    TItem.ImageIndex := 0;
                  ShortName := TPath.GetFileNameWithoutExtension(AList.Items[I].FileName);
                  while Length(TPath.GetExtension(ShortName)) > 1 do
                    begin
                      ShortName := TPath.GetFileNameWithoutExtension(Shortname);
                    end;
                  TItem.Text := ShortName;
                  TItem.Parent := GItem;
                  TItem.OnClick := TreeViewItemClick;
                  TheFileList.Add(AList.Items[I]);
                  Inc(Added);
                end;
              GItem.Parent := TreeView1;
              GItem.Expand;
              TreeView1.EndUpdate;
              if Added > 0 then
                begin
                  Elapsed := SecondsBetween(Start, Now);
                  {$IF defined(MSWINDOWS)}
                  WriteLnLog(Format('Memory Used = %d',[GetMemoryUsed]));
                  {$ENDIF}
                  WriteLnLog(Format('Scan of %s took %d secs',[SelDir, Elapsed]));
                  SystemSettings.SearchDir := SelDir;
    //              SaveModelList('../../models-test.json', AList);
                end;
            finally
              FreeAndNil(LoadingForm);
            end;
          end;
      finally
        AList.Free;
      end;
    end;
  MemStat;
end;

procedure TForm1.mnuSaveImageClick(Sender: TObject);
var
  frame: TFrameExport;
begin
  if Assigned(CastleApp) then
    begin
      frame := TFrameExport.Create(Self, 256,256);
      frame.GrabFromCastleApp(CastleApp);
      frame.Save('../../test.png');
      frame.Clear;
      FreeAndNil(frame);
    end;
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
      CheckGLTFTextures(SelDir);
      SystemSettings.SearchDir := SelDir;
    end;
end;

{$IF defined(MSWINDOWS)}
procedure TForm1.MemStat;
var
  vGlobalMemoryStatus : TMemoryStatus;
  memUsed: UInt64;
begin
  Memo1.Lines.Clear;
  GlobalMemoryStatus( vGlobalMemoryStatus );
  memUsed := GetMemoryUsed;

  with Memo1.Lines, vGlobalMemoryStatus do
    begin
      Add(Format('MemoryLoad  : %d%%',[dwMemoryLoad]));
      Add(Format('TotalPhys   : %8.3fG',[dwTotalPhys / (1024 * 1024 * 1024)]));
      Add(Format('AvailPhys   : %8.3fG', [dwAvailPhys / (1024 * 1024 * 1024)]));
      if memUsed < (1024 * 1024 * 1024) then
        Add(Format('Memory Used : %8.3fM',[memUsed / (1024 * 1024)]))
      else
        Add(Format('Memory Used : %8.3fG',[memUsed / (1024 * 1024 * 1024)]));
    end;
end;
{$ENDIF}

procedure TForm1.mnuCheckMemClick(Sender: TObject);
begin
  {$IF defined(MSWINDOWS)}
  MemStat;
  {$ENDIF}
end;

procedure TForm1.mnuClearClick(Sender: TObject);
begin
  if Assigned(CastleApp) then
    CastleApp.RemoveModels;
end;

procedure TForm1.ModelMove(const OX, OY, OZ: Single);
var
  model: TCastleModel;
begin
  if Assigned(CastleApp) then
  begin
    model := CastleApp.SelectedModel;
    if model <> Nil then
    begin
      model.Translation := model.Translation + Vector3(OX,OZ,OY);
      UpdateModelInfo;
    end;
  end;
end;

procedure TForm1.CameraMove(const OX, OY: Single);
begin
  if Assigned(CastleApp) and Assigned(CastleApp.Camera) then
  begin
    CastleApp.Camera.Pan := CastleApp.Camera.Pan + Vector2(OX,OY);
    UpdateCamInfo;
  end;
end;

procedure TForm1.NewModel(Sender: TObject; const AModel: TCastleModel);
begin
  UpdateModelInfo;
//  CastleApp.ResizeView;
  UpdateCamInfo;
end;

procedure TForm1.InclineCamera(Sender: TObject; const ARotation: Single);
begin
  CastleApp.Inclination := ARotation;
  UpdateCamInfo;
end;

procedure TForm1.RotateCamera(Sender: TObject; const ARotation: Single);
begin
  CastleApp.Azimuth := ARotation;
  UpdateCamInfo;
end;

procedure TForm1.RotateModel(Sender: TObject; const ARotation: Single);
var
  model: TCastleModel;
begin
  model := CastleApp.SelectedModel;
  if model <> Nil then
  begin
    model.Rotation := Vector4(0,1,0,ARotation);
    UpdateModelInfo;
    UpdateCamInfo;
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
  UpdateModelInfo;
  UpdateCamInfo;
end;

procedure TForm1.TabCameraClick(Sender: TObject);
begin
  CastleControl.SetFocus;
  UpdateCamInfo;
end;

procedure TForm1.TabModelClick(Sender: TObject);
begin
  CastleControl.SetFocus;
  UpdateModelInfo;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Assigned(CastleApp) and CastleApp.IsReady then
    begin
      NewModel(Self, Nil);
      Timer1.Enabled := False;
    end;
end;

procedure TForm1.TreeViewItemClick(Sender: TObject);
var
  TItem: TTreeViewItem;
  Obj: TObject;
  FI: TFileDirectory;
  fn: String;
  mi: TModelInfo;
  model: TCastleModel;
begin
  if Sender is TTreeViewItem then
    begin
      TItem := Sender as TTreeViewItem;
      Obj := TItem.TagObject; // TFileDirectory
      if Obj is TFileDirectory then
        begin
          FI := Obj as TFileDirectory;
          fn := FI.GetFullFileName;
          mi := FI.ModelInfo;
          if FileExists(fn) then
            begin
              if ReportMemoryLeaksOnShutdown then
                Caption := APPNAME + ': ' + fn + ' (ReportMemoryLeaksOnShutdown)'
              else
                Caption := APPNAME + ': ' + fn;
              if Assigned(mi) and Assigned(mi.model) then
                begin
                  model := CastleApp.CloneModel(mi.model);
                  if Assigned(model) then
                    begin
                      if TItem.TagString <> String.Empty then
                        begin
                          model.ForceAnimationPose(TItem.TagString, 0, True);
                    //      model.StopAnimation;
                        end;
                    end;
                end
              else
                CastleApp.AddModel(fn);
            end;
        end;
    end;
end;

end.

