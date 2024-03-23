unit InfoForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrmInfoDialog = class(TForm)
    FormText: TLabel;
    Panel1: TPanel;
    BtnOK: TButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    { Private declarations }
    procedure FormReset;
  public
    { Public declarations }
    procedure Setup(const MessageText: String);
  end;

implementation

{$R *.fmx}

procedure TfrmInfoDialog.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmInfoDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmInfoDialog.Setup(const MessageText: String);
begin
  FormReset;
  FormText.Text := MessageText;
end;

procedure TfrmInfoDialog.FormReset;
begin
  BtnOK.Enabled := True;
  ModalResult := mrNone;
end;


end.
