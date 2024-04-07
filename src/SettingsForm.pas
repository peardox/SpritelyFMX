unit SettingsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Edit, FMX.EditBox,
  FMX.SpinBox;

type
  TfrmSettingsDialog = class(TForm)
    Panel1: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    swUpDownAction: TSwitch;
    cbxThumbConfig: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    cbxPreloadConfig: TComboBox;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    swConstantSpeed: TSwitch;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    spnFPSprite: TSpinBox;
    spnFPSec: TSpinBox;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure swConstantSpeedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure FormReset;
    procedure EnableCorrectSwitch;
  public
    { Public declarations }
    procedure Setup(const TextOK: String = 'Save'; const TextCancel: String = 'Cancel');
  end;

implementation

uses Math, Sprite3DSettings;

{$R *.fmx}

procedure TfrmSettingsDialog.BtnOKClick(Sender: TObject);
begin
  SystemSettings.Config.Picker.UpDownLoadsModel := swUpDownAction.IsChecked;
  if (cbxThumbConfig.ItemIndex >= Ord(Low(TThumbnailOption))) and (cbxThumbConfig.ItemIndex <= Ord(High(TThumbnailOption))) then
      SystemSettings.Config.Picker.ThumbConfig := TThumbnailOption(cbxThumbConfig.ItemIndex)
  else
      raise Exception.Create('Value out of TThumbnailOption range');
  if (cbxPreloadConfig.ItemIndex >= Ord(Low(TPreloadOption))) and (cbxPreloadConfig.ItemIndex <= Ord(High(TPreloadOption))) then
      SystemSettings.Config.Picker.PreloadConfig := TPreloadOption(cbxPreloadConfig.ItemIndex)
  else
      raise Exception.Create('Value out of TPreloadOption range');

  SystemSettings.Config.Animation.FramesPerSecond := Floor(spnFPSec.Value);
  SystemSettings.Config.Animation.ConstantFramesPerSprite := swConstantSpeed.IsChecked;
  SystemSettings.Config.Animation.FramesPerSprite := Floor(spnFPSprite.Value);

  ModalResult := mrOK;
end;

procedure TfrmSettingsDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmSettingsDialog.Setup(const TextOK: String = 'Save'; const TextCancel: String = 'Cancel');
begin
  FormReset;
  BtnOK.Text := TextOK;
  BtnCancel.Text := TextCancel;
end;

procedure TfrmSettingsDialog.swConstantSpeedClick(Sender: TObject);
begin
  EnableCorrectSwitch;
end;

procedure TfrmSettingsDialog.EnableCorrectSwitch;
begin
  if swConstantSpeed.IsChecked then
    begin
      spnFPSec.Enabled := False;
      spnFPSprite.Enabled := True;
    end
  else
    begin
      spnFPSec.Enabled := True;
      spnFPSprite.Enabled := False;
    end;
end;

procedure TfrmSettingsDialog.FormCreate(Sender: TObject);
begin
  Caption := APPNAME + ' : Settings';
end;

procedure TfrmSettingsDialog.FormReset;
begin
  BtnOK.Enabled := True;
  BtnCancel.Enabled := True;
  ModalResult := mrNone;

  swUpDownAction.IsChecked := SystemSettings.Config.Picker.UpDownLoadsModel;
  cbxThumbConfig.ItemIndex := Ord(SystemSettings.Config.Picker.ThumbConfig);
  cbxPreloadConfig.ItemIndex := Ord(SystemSettings.Config.Picker.PreloadConfig);

  spnFPSec.Value := SystemSettings.Config.Animation.FramesPerSecond;
  swConstantSpeed.IsChecked := SystemSettings.Config.Animation.ConstantFramesPerSprite;
  spnFPSprite.Value := SystemSettings.Config.Animation.FramesPerSprite;

  EnableCorrectSwitch;
end;


procedure TfrmSettingsDialog.FormShow(Sender: TObject);
begin
  Application.ProcessMessages;
end;

end.
