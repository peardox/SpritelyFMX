unit MainGui;

{$define ClearOnScan}
{$define showfree}

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
  Sprite3DCheckTextures,
  Sprite3DSettings,
  X3DFields,
  X3DTIme,
  FrameToImage,
  ModelManager,
  Sprite3DTypes, FMX.Objects, FMX.ListBox, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.TreeView, System.ImageList, FMX.ImgList, FMX.Media
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
    LayoutRight: TLayout;
    View3D: TLayout;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    TabControl2: TTabControl;
    TabModel: TTabItem;
    Layout2D3D: TLayout;
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
    PickerImages: TImageList;
    TabCamera: TTabItem;
    StringGrid2: TStringGrid;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    LayoutPreview: TLayout;
    mnuCheckMem: TMenuItem;
    Label1: TLabel;
    ImageControl1: TImageControl;
    TabSprite: TTabItem;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    mnuViewport: TMenuItem;
    mnuAutofit: TMenuItem;
    mnuView3D: TMenuItem;
    mnuZoomAndPan: TMenuItem;
    mnuResetZoomAndPan: TMenuItem;
    mnuUseModelCenter: TMenuItem;
    mnuDAEDir: TMenuItem;
    mnuSTLDir: TMenuItem;
    mnuOptions: TMenuItem;
    mnuExit: TMenuItem;
    MediaImages: TImageList;
    mnuAutoRotate: TMenuItem;
    mnuAutoRotateWorld: TMenuItem;
    mnuAutoRotateModel: TMenuItem;
    Panel1: TPanel;
    btnPlay: TButton;
    btnPause: TButton;
    ArcDial1: TArcDial;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LayoutLeftResize(Sender: TObject);
    procedure SwitchView;
    procedure LayoutRightResize(Sender: TObject);
    procedure mnuLoadClick(Sender: TObject);
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
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure mnuSaveImageClick(Sender: TObject);
    procedure mnuCheckMemClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure mnuAutofitClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure mnuZoomAndPanClick(Sender: TObject);
    procedure mnuUseModelCenterClick(Sender: TObject);
    procedure mnuResetZoomAndPanClick(Sender: TObject);
    procedure mnuOBJDirClick(Sender: TObject);
    procedure mnuSTLDirClick(Sender: TObject);
    procedure mnuDAEDirClick(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure mnuAutoRotateClick(Sender: TObject);
    procedure ArcDial1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
    mm: TModelManager;
    ExeParam: String;
    PickedModel: TModelInfo;
    CastleControl: TCastleControl;
    CastleApp: TCastleApp;
    OutputSize: TUnitConfig;
    ManualLoadNode: TTreeViewItem;
//    TheFileList: TObjectList<TFileDirectory>;
    procedure NewModel(Sender: TObject; const AModel: TCastleModel);
    procedure LogTicker(Sender: TObject; const Msg: String);
    procedure RotateModel(Sender: TObject; const ARotation: Single);
    procedure RotateCamera(Sender: TObject; const ARotation: Single);
    procedure InclineCamera(Sender: TObject; const ARotation: Single);
    procedure UpdateModelInfo;
    procedure UpdateCamInfo(Sender: TObject);
    procedure CastleClick(Sender: TObject);
    procedure CameraMove(const OX, OY: Single);
    procedure LoadStyle(const AStyle: String = '');
    procedure PopulateProjections;
    procedure TreeViewItemClick(Sender: TObject);
    function AddModelNode(const ParentItem: TTreeViewItem;
      const mi: TModelInfo; const AForm: TForm = Nil): TTreeViewItem;
    procedure ManualLoad(const AFilename: String);
    function CreateThumbNail(const mi: TModelInfo; const AniIndex: Integer; const AniTime: Single = 0): Boolean;
    procedure MemStat(const OnlyNow: Boolean = False);
    function ScanModelDirClick(SelDir: String; AList: TObjectList<TFileDirectory>): Integer;
    procedure ResetZoomAndPan;
    procedure SetAutoRotate;
    procedure ReceivedElapsedTime(const Event: TX3DEvent;
      const Value: TX3DField; const Time: TX3DTime);
    function Analyse(const mi: TModelInfo; const Rotations, AniIndex: Integer;
      const AniTime: Single): Boolean;
    procedure ManualAnalyse;
  public
    { Public declarations }
  end;

  function GetMemoryUsed: UInt64;

var
  Form1: TForm1;

implementation

uses
  FastMM4,
  {$IF defined(MSWINDOWS)}
  Windows,
  {$ENDIF}
  Math,
  LoadingForm,
  InfoForm,
  SettingsForm,
  System.DateUtils,
  System.IOUtils,
  CastleLog,
  X3DNodes,
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
            UpdateModelInfo;
        end;
    end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Label3.Text := Format('%4.2f',[TrackBar1.Value]);
  CastleApp.Inclination := DegToRad(-TrackBar1.Value);
end;

procedure TForm1.ArcDial1Change(Sender: TObject);
var
  MappedVal: Single;
begin
  MappedVal := fmod(-ArcDial1.Value + 360, 360);
  Label2.Text := FloatToStr(MappedVal);
  CastleApp.Azimuth := DegToRad(MappedVal);
end;

procedure TForm1.cbxProjectionsChange(Sender: TObject);
begin
  if Assigned(CastleApp) then
    begin
      if (cbxProjections.ItemIndex >= 0) and (cbxProjections.ItemIndex < Length(SystemSettings.Projections)) then
        begin
          SystemSettings.Config.Viewport.DefaultView := cbxProjections.ItemIndex;
          TrackBar1.Value := RadToDeg((Pi/2) - SystemSettings.Projections[cbxProjections.ItemIndex].Inclination);
          ArcDial1.Value := RadToDeg(-SystemSettings.Projections[cbxProjections.ItemIndex].Azimuth);
        end;
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

  if Length(SystemSettings.Projections) > (SystemSettings.Config.Viewport.DefaultView - 1) then
    cbxProjections.ItemIndex := SystemSettings.Config.Viewport.DefaultView;
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
//      NewStyle := SystemSettings.StyleData + 'RubyGraphite.style';
//      NewStyle := SystemSettings.StyleData + 'Radiant.Win.style';
//      NewStyle := SystemSettings.StyleData + 'Vapor.Win.style';
//      NewStyle := SystemSettings.StyleData + 'Win10ModernBlue.style';
//      NewStyle := SystemSettings.StyleData + 'Sterling.Win.style';
//      NewStyle := SystemSettings.StyleData + 'MaterialOxfordBlue_Win.style';
      NewStyle := SystemSettings.StyleData + 'Win10ModernDark.style';
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
  mm := TModelManager.Create(Self);
  ExeParam := ParamStr(1);

  PickedModel := Nil;
  OutputSize := UnitConfig(256, 256);
  CastleControl := TCastleControl.Create(View3D);
  CastleControl.Align := TAlignLayout.Client;
  CastleControl.Parent := View3D;
  CastleControl.OnClick := CastleClick;
  CastleApp := TCastleApp.Create(CastleControl);
  CastleApp.OnExtMessage := LogTicker;
  CastleApp.OnModel := NewModel;
  CastleApp.OnCameraChange := UpdateCamInfo;
  ManualLoadNode := Nil;

  CastleControl.Container.View := CastleApp;

  LoadStyle;

  if ReportMemoryLeaksOnShutdown then
    Caption := APPNAME + ' (ReportMemoryLeaksOnShutdown)'
  else
    Caption := APPNAME;

  mnuView3D.IsChecked := False;
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

  StringGrid2.RowCount := 15;
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
  StringGrid2.Cells[0,14] := 'Tile Angle';

  mnuAutofit.IsChecked := SystemSettings.Config.Viewport.AutoFit;
  mnuZoomAndPan.IsChecked := SystemSettings.Config.Viewport.ZoomAndPan;
  mnuUseModelCenter.IsChecked := SystemSettings.Config.Viewport.UseModelCenter;

  UpdateModelInfo;
  UpdateCamInfo(Self);
  TabControl2.ActiveTab := TabCamera;

  PopulateProjections;

  Timer1.Enabled := True;
  Timer1.Interval := 100;
//  TheFileList := TObjectList<TFileDirectory>.Create;

  LayoutViewResize(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
{$ifdef showfree}
  WriteLnLog('Freeing TForm1');
{$endif}
//  if Assigned(TheFileList) then
//    TheFileList.Free;
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
    end;
  end else if TabControl2.ActiveTab = TabCamera then
  begin
    if mnuZoomAndPan.IsChecked then
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
  end;

  UpdateModelInfo;
  UpdateCamInfo(Self);

end;

procedure TForm1.UpdateModelInfo;
var
  model: TCastleModel;
  V: TViewStats;
begin
  model := CastleApp.SelectedModel;
  if Assigned(model) then
  begin
    StringGrid1.BeginUpdate;
    if CastleApp.IsReady then
      begin
//        CastleApp.Stage.Center := Vector3(0,0,0);
        V := CastleApp.Viewport.GetAxis(model);
        StringGrid1.Cells[1,0] := FormatFloat('###0.000', V.Box.View2D.Width);
        StringGrid1.Cells[1,1] := FormatFloat('###0.000', V.Box.View2D.Height);
        StringGrid1.Cells[1,2] := Format('%-4.0f x %-4.0f', [CastleControl.Width, CastleControl.Height]);
        {
        StringGrid1.Cells[1,5] := FormatFloat('###0.000', (V.Box.View2D.Width / 2) - V.Box.View2D.Left);
        StringGrid1.Cells[1,6] := FormatFloat('###0.000', (V.Box.View2D.Height / 2) - V.Box.View2D.Top);
        StringGrid1.Cells[1,7] := '';
        }
      end;

    StringGrid1.Cells[1,3] := FormatFloat('###0.000', model.Rotation.W);
    StringGrid1.Cells[1,4] := FormatFloat('###0.000', model.Scale.X);

    StringGrid1.Cells[1,5] := FormatFloat('###0.000', model.Center.X * model.Scale.X);
    StringGrid1.Cells[1,6] := FormatFloat('###0.000', model.Center.Y * model.Scale.X);
    StringGrid1.Cells[1,7] := FormatFloat('###0.000', model.Center.Z * model.Scale.X);

    StringGrid1.Cells[1,8] := FormatFloat('###0.000', model.BoundingBox.Size.X * model.Scale.X);
    StringGrid1.Cells[1,9] := FormatFloat('###0.000', model.BoundingBox.Size.Y * model.Scale.X);
    StringGrid1.Cells[1,10] := FormatFloat('###0.000', model.BoundingBox.Size.Z * model.Scale.X);
    StringGrid1.Cells[1,11] := TPath.GetFileNameWithoutExtension(model.Name);

    StringGrid1.Cells[1,12] := FormatFloat('###0.000', CastleApp.Zoom);
    StringGrid1.Cells[1,13] := FormatFloat('###0.00', RadToDeg(CastleApp.FieldOfView));
    StringGrid1.EndUpdate;
  end;

end;

procedure TForm1.UpdateCamInfo(Sender: TObject);
var
  cam: TCastleCamera;
begin
  if Assigned(CastleApp) then
  begin
    cam := CastleApp.Camera.Camera;
//    CastleApp.ApplyView;
    if Assigned(cam) then
      begin
        StringGrid2.BeginUpdate;
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
        StringGrid2.Cells[1,14] := FormatFloat('###0.000', CastleApp.DyDx);
        StringGrid2.EndUpdate;
      end;
  end;
end;

procedure TForm1.LayoutLeftResize(Sender: TObject);
begin
  Memo1.Height := 64;
  Memo1.Width := LayoutLeft.Width;
  ImageControl1.Width := LayoutLeft.Width;
  ImageControl1.Height := ImageControl1.Width;
  LayoutPreview.Width := LayoutLeft.Width;
  LayoutPreview.Height := 240;
  TreeView1.Width := LayoutLeft.Width;
  TreeView1.Height := LayoutLeft.Height - LayoutPreview.Height - ImageControl1.Height;
end;

procedure TForm1.LayoutViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  factor: Integer;
begin
  if mnuZoomAndPan.IsChecked then
    begin
      factor := 1;
      if WheelDelta > 0 then
        CastleApp.ZoomOut(factor);
      if WheelDelta < 0 then
        CastleApp.ZoomIn(factor);
      UpdateModelInfo;
      UpdateCamInfo(Self);
    end;
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
  Memo1.BeginUpdate;
  Memo1.Lines.Clear;
  Memo1.Lines.Add(Msg);
  Memo1.EndUpdate;
end;

function TForm1.AddModelNode(const ParentItem: TTreeViewItem; const mi: TModelInfo; const AForm: TForm = Nil): TTreeViewItem;
var
  TItem: TTreeViewItem;
  A: Integer;
  AItem: TTreeViewItem;
  Shortname: String;
  reqContinue: Boolean;
begin
  reqContinue := True;
  TItem := TTreeViewItem.Create(Self);
  TItem.TagObject := mi;
  TItem.Tag := -1;
  if mi.Hash <> String.Empty then
    begin
      if AForm <> Nil then
        begin
          if (SystemSettings.Config.Picker.ThumbConfig = TThumbnailOption.AlwaysThumbnail) and CreateThumbNail(mi, -1) then
            begin
              while reqContinue do
                reqContinue := TfrmLoadingDialog(AForm).AddMessage('  Creating Thumbnail for ' + mi.Name, 1);
            end;
        end;
    end;
  if mi.AnimationCount > 0 then
    begin
      TItem.ImageIndex := 1;
      for A := 0 to mi.AnimationCount - 1 do
        begin
          AItem := TTreeViewItem.Create(Self);
          AItem.Tag := A;
          AItem.TagObject := mi;
          AItem.ImageIndex := 2;
          AItem.Text := mi.Animations[A];
          AItem.Parent := TItem;
          AItem.OnClick := TreeViewItemClick;
          if AForm <> Nil then
            if reqContinue and (SystemSettings.Config.Picker.ThumbConfig = TThumbnailOption.AlwaysThumbnail) then
              reqContinue := TfrmLoadingDialog(AForm).AddMessage('  Adding Animation ' + mi.Model.AnimationsList[A] + ' for ' + mi.Name, 2);

          if (mi.Hash <> String.Empty) and Assigned(mi.model) then
            begin
              if AForm <> Nil then
                begin
                  if reqContinue and (SystemSettings.Config.Picker.ThumbConfig = TThumbnailOption.AlwaysThumbnail) and CreateThumbNail(mi, A, -0.5) then
                    reqContinue := TfrmLoadingDialog(AForm).AddMessage('    Creating Thumbnail for ' + mi.Model.AnimationsList[A], 1);
                end;
            end;

        end;
    end
  else
    TItem.ImageIndex := 0;
  ShortName := TPath.GetFileNameWithoutExtension(mi.Name);
  while Length(TPath.GetExtension(ShortName)) > 1 do
    begin
      ShortName := TPath.GetFileNameWithoutExtension(Shortname);
    end;
  TItem.Text := ShortName;
  TItem.Parent := ParentItem;
  TItem.OnClick := TreeViewItemClick;

  if reqContinue then
    Result := TItem
  else
    Result := Nil;
end;

function TForm1.CreateThumbNail(const mi: TModelInfo; const AniIndex: Integer; const AniTime: Single = 0): Boolean;
var
  frame: TFrameExport;
  fn: String;
begin
  Result := False;
  if AniIndex < 0 then
    fn := SystemSettings.ThumbData + mi.Hash + SystemSettings.ThumbType
  else
    fn := SystemSettings.ThumbData + mi.Hash + '-' + LeftPad(AniIndex) + SystemSettings.ThumbType;
  if not FileExists(fn) then
    begin
      if Assigned(CastleApp) then
        begin
          frame := TFrameExport.Create(Self, 256, 256);
          frame.ThumbFromCastleApp(CastleApp, mi.Model, AniIndex, AniTime);
          frame.Save(fn);
{$ifdef showfree}
          if not FileExists(fn) then
            WriteLnLog('Thumb not created (CreateThumbNail) ' + fn);
{$endif}
          frame.Clear;
          FreeAndNil(frame);
          Result := True;
        end;
    end;
end;

procedure TForm1.ManualLoad(const AFilename: String);
var
  model: TCastleModel;
  mi: TModelInfo;
begin
  TreeView1.CollapseAll;
  TreeView1.BeginUpdate;
  if ManualLoadNode = Nil then
    begin
      ManualLoadNode := TTreeViewItem.Create(Self);
      ManualLoadNode.Text := 'Opened Models';
      ManualLoadNode.Parent := TreeView1;
      ManualLoadNode.Expand;
    end;
  model := CastleApp.PreloadModel(AFileName);
  if Assigned(model) and not model.BoundingBox.IsEmpty then
     begin
      mi := model.SetInfo;
      mm.AddHash(mi.Hash);
      AddModelNode(ManualLoadNode, mi);
//      FreeAndNil(mi.model);     // Preload never
     end
  else
    begin
      WriteLnLog('Load Failure for ' + AFileName);
      FreeAndNil(model);
    end;
  TreeView1.EndUpdate;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  ManualAnalyse;
//  SwitchView;
end;

function TForm1.ScanModelDirClick(SelDir: String; AList: TObjectList<TFileDirectory>): Integer;
var
  LoadingForm: TfrmLoadingDialog;
  I: Integer;
  GItem: TTreeViewItem;
  Added: Integer;
  model: TCastleModel;
  mi: TModelInfo;
  Start: TDateTime;
  Elapsed: int64;
  reqContinue: Boolean;
  iForm: TfrmInfoDialog;

  procedure ShowProgress(const SelDir: String);
  var
    MemStr: String;
    memUsed: UInt64;
  begin
    memUsed := GetMemoryUsed;
    if memUsed < (1024 * 1024 * 1024) then
      MemStr := Format('Memory Used : %8.3fM',[memUsed / (1024 * 1024)])
    else
      MemStr := Format('Memory Used : %8.3fG',[memUsed / (1024 * 1024 * 1024)]);
    if ReportMemoryLeaksOnShutdown then
      Caption := APPNAME + ': Scanning ' + SelDir + ' (ReportMemoryLeaksOnShutdown) - ' + MemStr + ' Used'
    else
      Caption := APPNAME + ': Scanning ' + SelDir + ' - ' + MemStr + ' Used';
  end;

begin
  reqContinue := True;
  Added := 0;
  Start := Now;
  if AList.Count > 0 then
    begin
      TreeView1.CollapseAll;
      TreeView1.BeginUpdate;
      LoadingForm := TfrmLoadingDialog.Create(Self);
      LoadingForm.Setup(AList.Count);
      try
        Application.ProcessMessages;
        LoadingForm.Show;
        GItem := TTreeViewItem.Create(Self);
        GItem.Text := TPath.GetFileName(AList.Items[0].ParentDir);
        for I := 0 to AList.Count - 1 do
          begin
            if reqContinue then
              begin
                ShowProgress(SelDir);
    //            WriteLnLog('Trying load for ' + AList.Items[I].GetFullFileName);
                model := CastleApp.PreloadModel(AList.Items[I].GetFullFileName);
                if Assigned(model) and not model.BoundingBox.IsEmpty then
                  begin
                    mi := model.SetInfo;
                    mm.AddHash(mi.Hash);
              //      WriteLnLog(mi.Name + ' = ' + IntToStr(SizeOf(mi.model)));
                    if mi.AnimationCount > 0 then
                      reqContinue := LoadingForm.AddMessage('Loading ' + mi.Name + ' (Animated)')
                    else
                      reqContinue := LoadingForm.AddMessage('Loading ' + mi.Name);
                    if reqContinue and (AddModelNode(GItem, mi, LoadingForm) = Nil) then
                      begin
                        reqContinue := False;
                        Inc(Added);
                        Break;
                      end;
    //                TheFileList.Add(AList.Items[I]);
                    Inc(Added);
                  end
                else
                  begin
                    WriteLnLog('Load Failure for ' + AList.Items[I].GetFullFileName);
    //                FreeAndNil(AList.Items[I]);
                    FreeAndNil(model);
                end;
              end;
          end;
        GItem.Parent := TreeView1;
        GItem.Expand;
        TreeView1.EndUpdate;
        if Added > 0 then
          begin
            Elapsed := SecondsBetween(Start, Now);
            if ReportMemoryLeaksOnShutdown then
              Caption := APPNAME + ': Scan took ' + IntToStr(Elapsed) + ' Seconds ' + SelDir + ' (ReportMemoryLeaksOnShutdown)'
            else
              Caption := APPNAME + ': Scan took ' + IntToStr(Elapsed) + ' Seconds ' + SelDir;
            SystemSettings.SearchDir := SelDir;
//              SaveModelList('../../test.spritely', AList);
          end;
      finally
        FreeAndNil(LoadingForm);
      end;
    end;
  if Added = 0 then
    begin
      iForm := TfrmInfoDialog.Create(Self);
      try
        iForm.Setup('No files found.');
        iForm.ShowModal();
      finally
        FreeAndNil(iForm);
      end;
    end;
  Result := Added;
end;

procedure TForm1.mnuSaveImageClick(Sender: TObject);
var
  frame: TFrameExport;
begin
  if Assigned(CastleApp) then
    begin
      frame := TFrameExport.Create(Self, 256, 256);
      frame.GrabFromCastleApp(CastleApp);
      frame.Save('../../test.png');
 //     frame.Clear;
      FreeAndNil(frame);
    end;
end;

procedure TForm1.mnuUseModelCenterClick(Sender: TObject);
begin
  mnuUseModelCenter.IsChecked := not mnuUseModelCenter.IsChecked;
  SystemSettings.Config.Viewport.UseModelCenter := mnuUseModelCenter.IsChecked;
end;

procedure TForm1.mnuZoomAndPanClick(Sender: TObject);
begin
  mnuZoomAndPan.IsChecked := not mnuZoomAndPan.IsChecked;
  SystemSettings.Config.Viewport.ZoomAndPan := mnuZoomAndPan.IsChecked;
end;

procedure TForm1.mnuLoadClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := SystemSettings.LoadFromDir;
  OpenDialog1.Filter:='GLTF Models|*.gltf;*.glb|OBJ Models|*.obj|DAE Models|*.dae|X3D Models|*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz|STL Models|*.stl';
  if OpenDialog1.Execute then
    begin
      if ReportMemoryLeaksOnShutdown then
        Caption := APPNAME + ': ' + OpenDialog1.FileName + ' (ReportMemoryLeaksOnShutdown)'
      else
        Caption := APPNAME + ': ' + OpenDialog1.FileName;
      ManualLoad(OpenDialog1.FileName);
//      CastleApp.AddModel(OpenDialog1.FileName);
      SystemSettings.LastModel := OpenDialog1.FileName;
    end;
end;

procedure TForm1.mnuDAEDirClick(Sender: TObject);
var
  Added: Integer;
  AList: TObjectList<TFileDirectory>;
  SelDir: String;
begin
  if SelectDirectory('Scan for DAE models (recursively)', SystemSettings.SearchDir, SelDir) then
    begin
      AList := ScanDAEModels(SelDir);
      try
        Added := ScanModelDirClick(SelDir, AList);
        if Added < AList.Count then
          WriteLnLog('Added ' + IntToStr(Added) + ' expecting ' + IntToStr(AList.Count));
      finally
        AList.Free;
      end;
  end;
end;

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.mnuGLTFDirClick(Sender: TObject);
var
  Added: Integer;
  AList: TObjectList<TFileDirectory>;
  SelDir: String;
begin
  if SelectDirectory('Scan for GLTF models (recursively)', SystemSettings.SearchDir, SelDir) then
    begin
      AList := ScanGLTFModels(SelDir);
      try
        Added := ScanModelDirClick(SelDir, AList);
        if Added < AList.Count then
          WriteLnLog('Added ' + IntToStr(Added) + ' expecting ' + IntToStr(AList.Count));
      finally
        AList.Free;
      end;
  end;
end;

procedure TForm1.mnuOBJDirClick(Sender: TObject);
var
  Added: Integer;
  AList: TObjectList<TFileDirectory>;
  SelDir: String;
begin
  if SelectDirectory('Scan for OBJ models (recursively)', SystemSettings.SearchDir, SelDir) then
    begin
      AList := ScanOBJModels(SelDir);
      try
        Added := ScanModelDirClick(SelDir, AList);
        if Added < AList.Count then
          WriteLnLog('Added ' + IntToStr(Added) + ' expecting ' + IntToStr(AList.Count));
      finally
        AList.Free;
      end;
  end;
end;

procedure TForm1.mnuOptionsClick(Sender: TObject);
var
  mr: TModalResult;
  sForm: TfrmSettingsDialog;
begin
  sForm := TfrmSettingsDialog.Create(Self);
  try
    sForm.Setup;
    mr := sForm.ShowModal();

    if mr = mrOK then
      begin
        SystemSettings.Save;
      end;
  finally
    FreeAndNil(sForm);
  end;
end;

procedure TForm1.mnuSTLDirClick(Sender: TObject);
var
  Added: Integer;
  AList: TObjectList<TFileDirectory>;
  SelDir: String;
begin
  if SelectDirectory('Scan for STL models (recursively)', SystemSettings.SearchDir, SelDir) then
    begin
      AList := ScanSTLModels(SelDir);
      try
        Added := ScanModelDirClick(SelDir, AList);
        if Added < AList.Count then
          WriteLnLog('Added ' + IntToStr(Added) + ' expecting ' + IntToStr(AList.Count));
      finally
        AList.Free;
      end;
  end;
end;

procedure TForm1.mnuResetZoomAndPanClick(Sender: TObject);
begin
  ResetZoomAndPan;
end;

procedure TForm1.ResetZoomAndPan;
begin
  if Assigned(CastleApp) and CastleApp.IsReady then
    begin
      CastleApp.FitViewToModel(CastleApp.Stage);
      CastleApp.Camera.Pan := Vector2(0, 0);
      UpdateModelInfo;
    end;
end;

procedure TForm1.mnuAutofitClick(Sender: TObject);
begin
  mnuAutofit.IsChecked := not mnuAutofit.IsChecked;
  SystemSettings.Config.Viewport.Autofit := mnuAutofit.IsChecked;
end;

procedure TForm1.mnuAutoRotateClick(Sender: TObject);
begin
  mnuAutoRotate.IsChecked := not mnuAutoRotate.IsChecked;
  SetAutoRotate;
end;

procedure TForm1.SetAutoRotate;
begin
  if Assigned(CastleApp) then
    begin
      if mnuAutoRotateModel.IsChecked and mnuAutoRotateWorld.IsChecked then
        CastleApp.AutoRotate := False;
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

procedure TForm1.MemStat(const OnlyNow: Boolean);
var
  vGlobalMemoryStatus : TMemoryStatus;
  memUsed: UInt64;
begin
  if OnlyNow then
    begin
      memUsed := GetMemoryUsed;
      if memUsed < (1024 * 1024 * 1024) then
        Memo1.Lines.Add(Format('Memory Used : %8.3fM',[memUsed / (1024 * 1024)]))
      else
        Memo1.Lines.Add(Format('Memory Used : %8.3fG',[memUsed / (1024 * 1024 * 1024)]));
{$ifdef showfree}
      if memUsed < (1024 * 1024 * 1024) then
        WriteLnLog(Format('Memory Used : %8.3fM',[memUsed / (1024 * 1024)]))
      else
        WriteLnLog(Format('Memory Used : %8.3fG',[memUsed / (1024 * 1024 * 1024)]));
{$endif}
    end
  else
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
end;

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
    UpdateCamInfo(Self);
  end;
end;

procedure TForm1.NewModel(Sender: TObject; const AModel: TCastleModel);
begin
  UpdateModelInfo;
//  CastleApp.ResizeView;
  UpdateCamInfo(Self);
end;

procedure TForm1.InclineCamera(Sender: TObject; const ARotation: Single);
begin
  CastleApp.Inclination := ARotation;
  Caption := 'Inc = ' + FloatToStr(RadToDeg(CastleApp.Inclination)) + ' : ' +
             'Azi = ' + FloatToStr(RadToDeg(CastleApp.Azimuth));
  UpdateCamInfo(Self);
end;

procedure TForm1.RotateCamera(Sender: TObject; const ARotation: Single);
begin
  CastleApp.Azimuth := ARotation;
  Caption := 'Inc = ' + FloatToStr(RadToDeg(CastleApp.Inclination)) + ' : ' +
             'Azi = ' + FloatToStr(RadToDeg(CastleApp.Azimuth));
  UpdateCamInfo(Self);
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
    UpdateCamInfo(Self);
  end;
end;

procedure TForm1.LayoutRightResize(Sender: TObject);
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

procedure TForm1.SwitchView;
begin
  mnuView3D.IsChecked := not mnuView3D.IsChecked;
  CastleApp.SwitchView3D(mnuView3D.IsChecked);
//  UpdateModelInfo;
//  UpdateCamInfo(Self);
end;

procedure TForm1.TabCameraClick(Sender: TObject);
begin
  CastleControl.SetFocus;
  UpdateCamInfo(Self);
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

procedure TForm1.TreeView1Change(Sender: TObject);
var
  TItem: TTreeViewItem;
  Obj: TObject;
  mi: TModelInfo;
  fn: String;
  NewThumb: Boolean;
begin
  TItem := TreeView1.Selected;
  Obj := TItem.TagObject;
  if Obj is TModelInfo then
    begin
      mi := Obj as TModelInfo;
      if Assigned(mi) and (mi.Hash <> String.Empty) then
        begin
          if TItem.Tag < 0 then
            fn := SystemSettings.ThumbData + mi.Hash + SystemSettings.ThumbType
          else
            fn := SystemSettings.ThumbData + mi.Hash + '-' + LeftPad(TItem.Tag) + SystemSettings.ThumbType;
          if FileExists(fn) then
            begin
              ImageControl1.LoadFromFile(fn);
            end
          else if (SystemSettings.Config.Picker.ThumbConfig <> TThumbnailOption.NeverThumbnail) then
            begin
              if TItem.Tag < 0 then
                NewThumb := CreateThumbNail(mi, -1)
              else
                NewThumb := CreateThumbNail(mi, TItem.Tag, -0.5);
              if NewThumb then
                begin
                  if FileExists(fn) then
                    begin
                      ImageControl1.LoadFromFile(fn);
                    end
                  else
                    begin
                      ImageControl1.LoadFromFile(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(SystemSettings.AppHome) + 'img') + 'nofile.png');
                    end;
                end;
            end
          else
            begin
{$ifdef showfree}
              WriteLnLog('Thumb not found (TreeView1Change) ' + fn);
{$endif}
              ImageControl1.LoadFromFile(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(SystemSettings.AppHome) + 'img') + 'nofile.png');
            end;
        end;
    end
  else
    ImageControl1.LoadFromFile(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(SystemSettings.AppHome) + 'img') + 'nofile.png');
end;

procedure TForm1.TreeView1KeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  TItem: TTreeViewItem;
  Obj: TObject;
  mi: TModelInfo;
begin
// WriteLnLog('Key = ' + IntToStr(Key) + ' = ' + KeyChar);
  if Key = 13 then
    begin
      TItem := TreeView1.Selected;
      Obj := TItem.TagObject;
      if Obj is TModelInfo then
        begin
          mi := Obj as TModelInfo;
          if Assigned(mi) and Assigned(mi.model) then
            begin
              if mi.model <> CastleApp.SelectedModel then
                begin
                  if ReportMemoryLeaksOnShutdown then
                    Caption := APPNAME + ': ' + mi.Name + ' (ReportMemoryLeaksOnShutdown)'
                  else
                    Caption := APPNAME + ': ' + mi.Name;
                  CastleApp.SwitchModel(mi.model);
                  if Assigned(mi.model) then
                    begin
                      if TItem.Tag <> -1 then
                        begin
                          mi.model.ForceAnimationPose(mi.Animations[TItem.Tag], 0, True);
                        end;
                    end;
                end;
            end;
        end;
    end;
end;

procedure TForm1.TreeViewItemClick(Sender: TObject);
var
  TItem: TTreeViewItem;
  Obj: TObject;
  mi: TModelInfo;
  AniTime: Single;
  AniName: String;
begin
  if Sender is TTreeViewItem then
    begin
      TItem := Sender as TTreeViewItem;
      Obj := TItem.TagObject;
      if Obj is TModelInfo then
        begin
          mi := Obj as TModelInfo;
          mi.ActiveAnimation := -1;
          if Assigned(mi) then
            begin
              if Assigned(PickedModel) and Assigned(PickedModel.Model) then
                begin
                  if PickedModel.Model <> mi.Model then
                    begin
                      PickedModel.Model.ResetAnimationState(Nil);
                    end
                  else
                    begin
//                      mi.Model.ResetAnimationState(Nil);
                    end;

                  PickedModel := mi;
                end
              else
                PickedModel := mi;
              {
              if not Assigned(mi.model) then
                begin
                  mi.Model := CastleApp.AddModel(mi.ModelFile);
                  if not Assigned(mi.model) or mi.model.BoundingBox.IsEmpty then
                     begin
                      Exit;
                     end
                end;
              }
              if ReportMemoryLeaksOnShutdown then
                Caption := APPNAME + ': ' + mi.Name + ' (ReportMemoryLeaksOnShutdown)'
              else
                Caption := APPNAME + ': ' + mi.Name;
              if Assigned(mi.model) then
                begin
                  CastleApp.SwitchModel(mi.model);
                  if TItem.Tag <> -1 then
                    begin
                      mi.ActiveAnimation := TItem.Tag;
                      AniName := mi.Animations[TItem.Tag];
                      AniTime := mi.Model.AnimationDuration(AniName) / 2;
                      mi.model.ForceAnimationPose(AniName, AniTime, True);
                    end;
                end;
            //  ResetZoomAndPan;
            end;
        end;
    end;
end;

procedure TForm1.btnPlayClick(Sender: TObject);
var
  model: TCastleModel;
  AniName: String;
//  TimeSensor: TTimeSensorNode;
begin
  if Assigned(PickedModel) then
    begin
      model := PickedModel.model;
      if Assigned(model) then
        begin
          if (PickedModel.AnimationCount > 0) and (PickedModel.ActiveAnimation >=0) then
            begin
              AniName := PickedModel.Animations[PickedModel.ActiveAnimation];
//              AniTime := mi.Model.AnimationDuration(AniName) / 2;
              model.ResetAnimationState;
//              TimeSensor := model.AnimationTimeSensor(AniName);
//              TimeSensor.EventElapsedTime.AddNotification(ReceivedElapsedTime);
              CastleApp.Frame := 0;
              if not model.PlayAnimation(AniName, True, True) then
                WriteLnLog('PlayAnimation returned false');

            end;
        end;
    end;
end;

procedure TForm1.btnPauseClick(Sender: TObject);
var
  model: TCastleModel;
//  AniName: String;
begin
  if Assigned(PickedModel) then
    begin
      model := PickedModel.model;
      if Assigned(model) then
        begin
          if (PickedModel.AnimationCount > 0) and (PickedModel.ActiveAnimation >=0) then
            begin
//              AniName := PickedModel.Animations[PickedModel.ActiveAnimation];
              model.StopAnimation;
            end;
        end;
    end;
end;

procedure TForm1.ReceivedElapsedTime(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  Val: Double;
begin
  Val := (Value as TSFTime).Value;
//  if Val >= AnimStop then
    begin
//      AnimNode.Stop;
      WriteLnLog('At ' + IntToStr(CastleApp.Frame) + ' ReceivedElapsedTime - ' + FloatToStr(Val));
    end;
end;

procedure TForm1.ManualAnalyse;
begin
  if Assigned(PickedModel) and Assigned(PickedModel.model) then
    begin
      Analyse(PickedModel, 4, -1, 0);
    end;

end;

function TForm1.Analyse(const mi: TModelInfo; const Rotations, AniIndex: Integer;
      const AniTime: Single): Boolean;
var
  frame: TFrameExport;
  SAP: TSizeAndPan;
begin
  Result := False;
  if Assigned(CastleApp) then
    begin
      frame := TFrameExport.Create(Self, 256, 256);
      SAP := frame.AnalyseModel(mi.Model, Rotations, AniIndex, AniTime);
      WriteLnLog('SAP = ' + FloatToStr(SAP.Size) + ' (' + FloatToStr(SAP.Pan.X) + ', ' + FloatToStr(SAP.Pan.Y) + ')');
      frame.Clear;
      FreeAndNil(frame);
      Result := True;
    end;
end;


end.

