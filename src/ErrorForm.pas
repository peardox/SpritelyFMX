unit ErrorForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrmErrorDialog = class(TForm)
    FormText: TLabel;
    Panel1: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    { Private declarations }
    procedure FormReset;
  public
    { Public declarations }
    procedure Setup(const MessageText: String; const TextOK: String = 'OK'; const TextCancel: String = 'Cancel');
  end;

implementation

{$R *.fmx}

procedure TfrmErrorDialog.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmErrorDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmErrorDialog.Setup(const MessageText: String; const TextOK: String = 'OK'; const TextCancel: String = 'Cancel');
begin
  FormReset;
  FormText.Text := MessageText;
  BtnOK.Text := TextOK;
  BtnCancel.Text := TextCancel;
end;

procedure TfrmErrorDialog.FormReset;
begin
  BtnOK.Enabled := True;
  BtnCancel.Enabled := True;
  ModalResult := mrNone;
end;


end.
