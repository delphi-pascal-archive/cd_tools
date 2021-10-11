unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CDTools, ExtCtrls, StdCtrls, Buttons, ImgList;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    Image12: TImage;
    Image13: TImage;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Bevel1: TBevel;
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    Bevel2: TBevel;
    Label4: TLabel;
    Bevel3: TBevel;
    Label5: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses AboutFrm;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
fDevices := TStringList.Create;
InitializeASPI(true);
ScanDevices;
ComboBox1.Items := fDevices;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
Close;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
AboutForm.showmodal;
end;

procedure TMainForm.ComboBox1Select(Sender: TObject);
begin
if ComboBox1.ItemIndex <> -1 then begin
  SelectDevice(ComboBox1.Text);
  SetDeviceID(fDevice);

//  GetDiskInformation;

  Label1.Caption := inttostr(MaxReadSpeed div 176)+' x';
  Label6.Caption := inttostr(MaxWriteSpeed div 176)+' x';

  CheckBox1.Checked := dcReadCDR in fDevCaps;
  CheckBox2.Checked := dcReadCDRW in fDevCaps;
  CheckBox3.Checked := dcReadDVDR in fDevCaps;
  CheckBox4.Checked := dcReadDVDRW in fDevCaps;
  CheckBox5.Checked := dcReadDVDRAM in fDevCaps;
  CheckBox6.Checked := dcReadDVDPLUSR in fDevCaps;
  CheckBox7.Checked := dcReadDVDPLUSRW in fDevCaps;
  CheckBox8.Checked := dcReadDVD in fDevCaps;

  CheckBox9.Checked := dcWriteCDR in fDevCaps;
  CheckBox10.Checked := dcWriteCDRW in fDevCaps;
  CheckBox11.Checked := dcWriteDVDR in fDevCaps;
  CheckBox12.Checked := dcWriteDVDRW in fDevCaps;
  CheckBox13.Checked := dcWriteDVDRAM in fDevCaps;
  CheckBox14.Checked := dcWriteDVDPLUSR in fDevCaps;
  CheckBox15.Checked := dcWriteDVDPLUSRW in fDevCaps;

end;
end;

end.
