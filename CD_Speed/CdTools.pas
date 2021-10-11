////////////////////////////////////////////////////////////////////////////
//									                                                      //
// CD Tools unit (BlackCash soft 2007)			                          	  //
// Autor - BlackCash                                      							  //
// EMail - BlackCash2006@Yandex.ru					                              //
//                                                    									  //
////////////////////////////////////////////////////////////////////////////
// При использовании данного модуля в своих проектах обязательно сделайте //
// ссылку на автора этого модуля или укажите в форме о программе имя      //
// "BlackCash", как одного из авторов. 					          							  //
////////////////////////////////////////////////////////////////////////////

unit CDTools;
interface
uses
  Windows, Messages, SysUtils, Classes, ASPI, Math, SyncObjs, CDConst, SPTI;

var
  CommandName: array[0..254] of string;
  GenerateLogs: Boolean = False;

const
  etQuick = 1;
  etComplete = 0;
  etTrack = 2;

  dsEmptyDisc = 0;
  dsIncompleteDisc = 1;
  dsCompleteDisc = 2;
  dsOther = 3;
  lsEmptySession = 0;
  lsIncompleteLastSession = 1;
  lsDamagedLastSession = 2;
  lsCompleteLastSession = 3;
  bfNoRewriteable = 0;
  bfError = 1;
  bfInProgress = 2;
  bfDone = 3;

  dtNoDevice = -1;
  dtUnKnown = -2;
  dtDirectAccess = 0;
  dtSeqAccess = 1;
  dtPrinter = 2;
  dtProcessor = 3;
  dtWORM = 4;
  dtCDROM = 5;

  mtUNKNOWN   = 0;
  mtCD_ROM    = 1;
  mtCD_R      = 2;
  mtCD_RW     = 3;
  mtDVD_ROM   = 4;
  mtDVD_R     = 5;
  mtDVD_RAM   = 6;
  mtDVD_RW    = 7;
  mtDVD_RW_RO = 8;
  mtDVD_RW_SR = 9;
  mtDVD_PLUSRW= 10;
  mtDVD_PLUSR = 11;
  mtDDCD_ROM  = 12;
  mtDDCD_R    = 13;
  mtDDCD_RW   = 14;

var
  DiscTypeString: array[0..14] of string = (
      'UNKNOWN',
      'CD-ROM',
      'CD-R',
      'CD-RW',
      'DVD-ROM',
      'DVD-R',
      'DVD-RAM',
      'DVD-RW',
      'DVD-RW',
      'DVD-RW',
      'DVD+RW',
      'DVD+R',
      'DDCD-ROM',
      'DDCD-R',
      'DDCD-RW');
type

  TScsiDeviceCapabilities = ( dcReadCDR,   dcReadCDRW,  dcReadMethod2, dcReadDVD,   dcReadDVDR,   dcReadDVDRW, dcReadDVDRAM, dcReadDVDPLUSR, dcReadDVDPLUSRW,
                              dcWriteCDR,  dcWriteCDRW, dcWriteTest,   dcWriteDVDR, dcWriteDVDRW, dcWriteDVDRAM, dcWriteDVDPLUSR, dcWriteDVDPLUSRW,
                              dcWriteISRC, dcUnderrunProtection );
  TDeviceCapabilities = set of TScsiDeviceCapabilities;
  TWriteMethod = (wmDAO, wmSAO, wmTAO);
  TWriteMethods = set of TWriteMethod;
  TEraseDoneEvent = procedure (Sender: TObject; WithError: Boolean) of object;
  TOnDebugMsg = procedure (Sender: TObject; Message: String; mType: Byte) of object;
  TDeviceChangedEvent = procedure (Sender: TObject) of object;

  TDisc = record
    Valid: Boolean;
    TotalBlocks: Cardinal;
    UsedBlocks: Cardinal;
    BlockLength: Cardinal;
    FirstCompleteSession,
    LastCompleteSession,
    DiscStatus,
    LastSession : Byte;
    DiscTypeCode: Word;
    DiscType: String;
    Eraseable: Boolean;
  end;
  type
  TSenseInfo = packed record
    ResponseCode: Byte;      // 0
    B1: Byte;                // 1
    SenseKey: Byte;          // 2
    Information: DWORD;      // 3-6
    AddSenseLen: Byte;       // 7
    CommandSpecific: DWORD;  // 8-11
    ASC: Byte;               // 12
    ASQ: Byte;               // 13
    FRUC: Byte;              // 14
    B15, B16, B17: Byte;     // 15-17
    B18n: array[0..16] of byte; // 18 -
    P: DWord;
    B19n: byte;              // 18 -
  end;

  TEraseThread = class(TThread)
  private
    fProgress: Integer;
    Medium: Byte;
    AproxSecs: Word;
    EraseDoneWithError: Boolean;
    procedure EraseDoneEvent;

  protected
    procedure Execute; override;
    property Progress: Integer read fProgress write fProgress;

  end;

var
  buf1: array[0..$ff0] of char;
  buf2: array[0..$ff00] of char;
  buf3: array[0..$FF] of byte;
  tmps: array[0..1000] of char;
  wms: Boolean = False;
  fwms: Boolean = False;
  _SenseInfo: TSenseInfo;
  DiscInfo: TDiscInformation;
  MediumIs: SmallInt;
  fImmedCT: Boolean;
    PDVR103,
    SonyCRX100E,
    TEAC512EB,
    SonyPowerBurn: Boolean;
    DontShowError, ConsoleApplication: Boolean;
    fDriveLetters: array[0..26] of string;
    fDeviceSubType: ShortInt;
    fHaID,
    fTarget,
    fLun: Byte;
    EraseType_: Byte;
    fAdapters: ShortInt;
    ModePage2A: TModePage2A;
    ModePage05: TModePage05;
    fUnderrunProtection,
    fTestWrite: Boolean;
    fErrorString,
    fDevice: string;
    fDeviceType: ShortInt;
    fDeviceName: string;
    fInquiryData: TInquiryData;
    fDoDebug: Boolean;
    fDevCaps: TDeviceCapabilities;
    fDevices: TStrings;
    fTargetBusy: Boolean;
    fOnDeviceChange:  TDeviceChangedEvent;
    fHAInquiry: TSRB_HAInquiry;
    fSCAbort: TSRB_Abort;
    fEraseDone: TEraseDoneEvent;
    fDisc: TDisc;
    fTrackInformation: TTrackInformationBlock;
    fTOC100: TTOCPMATIP0100;
    fDeviceMaxWriteSpeed,
    fDeviceMaxReadSpeed: Word;
    fDeviceBufferSize, fDeviceFreeBufferSize: Cardinal;
    fFinalizeDisc, fErasing: Boolean;
    fLastSense: TSenseArea;
    fOnDebugMsg: TOnDebugMsg;
    fWriteMethod: TWriteMethod;
    fWriteMethods: TWriteMethods;
    fReadSpeed, fWriteSpeed: Word;
    CriticalSection: TCriticalSection;
    
    function GetConfigData(RT: Byte; Profile: Word; buf: Pointer; buflen: Integer): Boolean;
    function GetDiskLetter(H, T, L: Byte): Byte;
    function InitializeASPI(InternalFirst: Boolean = False; AspiPath: String = ''): Boolean;
    function DeInitializeASPI: Boolean;
    function GetDiskInformation: Boolean;
    function ExecScsiCommand(var sc: TSRB_ExecSCSICmd; TimeOut: Integer): Integer;
    function ModeSense10(PageCode: Byte; var buf: Array of byte; buflen: Integer; PS: Boolean = False): Boolean;
    function ModeSelect10(Buffer: pchar; buflen: Integer): Boolean;
    function SetStreaming(Buffer: Pointer; BufLength: WORD): Boolean;
    function SendCueSheet(buffer: PChar; BufferLength: Integer): Boolean;
    function ReadDVDStructure(Layer, Format: Byte; Buf: Pointer; BufLength: Word): Boolean;
    function SendDVDStructure(Format: Byte; Buf: Pointer; BufLength: Word): Boolean;
    function SendDVDStructureTimeStamp(Time: TDateTime): Boolean;
    function Inquiry: Boolean;
    function SessionsOnDisc: Smallint;
    function UsedBlocksOnDisc: Cardinal;
    function FreeBlocksOnDisc: Cardinal;
    function TotalBlocksOnDisc: Cardinal;
    function Erasable: Boolean;
    function DiscType: Byte;
    function ScsiReset: Integer;
    function DiscStatus: SmallInt;
    function LastSessionStatus: SmallInt;
    function GetDeviceCapabilities(Physical: Boolean = False): Boolean;
    function IsJustLinkCapable: Boolean;
    function SetWriteParams(TestWrite, UnderrunProtected, MultiSession: Boolean; MediumIs: SmallInt): Boolean;
    function GetLastRecordedAddress: Cardinal;
    function DecodeSense(scm : TSRB_ExecSCSICmd): string;
    function GetAspiError(Status, HaStat, TargStat : BYTE) : string;
    function AdditionalSenseInformation(scm : TSRB_ExecSCSICmd): String;
    function MaxWriteSpeed: Word;
    function MaxReadSpeed: Word;
    function CurrentWriteSpeed: Word;
    function CurrentReadSpeed: Word;
    function TestUnitReady(TimeOut: Integer = 5000): Boolean;
    function WaitForReady(TimeOut: Integer = 10000; Step: Integer = 5000): Boolean;
    function LockMedium(Unlock: Boolean = False): Boolean;
    procedure LockDrive(UnLock: Boolean = True);
    function LoadMedium(Eject: Boolean=False): Boolean;
    function EraseDisc(EraseType: Byte = etQuick): Boolean;
    function ReadCapacity(var Capacity, SectorSize: Cardinal): Boolean;
    function ReserveTrack(ReservationSize: Cardinal): Boolean;
    function Read10(LBA: Cardinal; TransferLength: Word; Buffer: PChar; buflen: Cardinal): Boolean;
    function ReadCD(LBA: Cardinal; TransferLength: Word; Buffer: PChar; buflen: Cardinal; SecType, Other, SubChannelsSelection: Byte): Boolean;
    function Write10(LBA: Cardinal; TransferLength: Word; Buffer: PChar; buflen: Cardinal; Flags: Byte = $50): Boolean;
    function FlushCache(TimeOut: Integer = 60 * 1000 * 5; Immed: Boolean = False): Boolean;
    function FormatUnit(FormatCode: Byte; Buffer: PChar; Length: Integer): Boolean;
    function CloseTrack(Session: Boolean=False; Immed: Boolean = True; DVDPR: Boolean = False; TrackNo: Byte = 0): Boolean;
    function CloseTrackDVD(Immed: Boolean; b2, b3, b4, b5: Byte): Boolean;
    function SetCDSpeed(ReadSpeed, WriteSpeed: Cardinal): Boolean;
    function SendOPC: Boolean;
    function ReadBufferCapacity(var BufferLength, BlankBufferLength: Cardinal): Boolean;
    function GetFormatCapacity(var Capacity: Cardinal; var SectorSize: Cardinal): Boolean;
    function ReadTrackInformation(TrackNumber: Byte): Boolean;
    function ReadTOC(Format: Byte; buffer: PChar; BufferLength: Integer; SessionTrackNumber: Byte = 0; Time: Boolean = False; Control: Byte = 0): Boolean;
    function ReadDiscInformation: Boolean;
    function ReadDiscInformationRaw(Buffer: PChar; BufferLength: Integer): Boolean;
    function RequestSense(Buffer: PChar; Len: Byte): Boolean;
    function GetDiscInformation: TDisc;
    function EraseProgress: Integer;
    function SelectDevice(sDevice: String): Boolean;
    function  GetHostAdapterInfo(HA: Byte): ShortInt;
    function  AbortSCSICommand(srb: TSRB_ExecSCSICmd): Boolean;
    function  GetASPIInitialized: Boolean;
    function  GetDriveLetter: Char;
    function  SetDeviceByDriveLetter(DriveLetter: Char): Boolean;
    function  GetWriteMethods: Boolean;
    function  CheckWriteMethod(wm: TWriteMethod): Boolean;
    procedure SetDriveLetter(Value: Char);
    procedure DeviceChanged;
    procedure SetDeviceID(Value: string);
    procedure SetWriteSpeed(Value: Word);
    procedure Lock;
    procedure Unlock;
    procedure Rewind;
    procedure ScanDevices;
function L2MDW(DW: DWORD) : DWORD;
function L2MW(W: WORD) : WORD;

implementation

type
  TConfigHeader = packed record
    DataLength: DWord;
    Res1: Word;
    CurrentProfile: Word;
    FeatureCode: Word;
    Version: Byte;
    AdditionalLength:  Byte;
    otherData: array[0..101] of byte
end;

var
  dtBuf: TConfigHeader;
  Wait02: Boolean;
function L2MW(W: WORD) : WORD;
begin
  result := ((W SHL 8) AND $FF00) OR ((W SHR 8) AND $00FF);
end;
function L2MDW(DW: DWORD) : DWORD;
begin
  result := ((DW SHL 24) AND $FF000000)
  OR ((DW SHL  8) AND $00FF0000)
  OR ((DW SHR  8) AND $0000FF00)
  OR ((DW SHR 24) AND $000000FF);
end;
procedure CvtEndians(const Src; var Dest; Count : integer);
var
  pSrc,
  pDst: PChar;
  i : integer;
begin
  pSrc := @Src;
  pDst := PChar(@Dest) + Count;
  for i := 0 to Count-1 do begin
    Dec(pDst);
    pDst^ := pSrc^;
    Inc(pSrc);
  end;
end;
function InitializeASPI(InternalFirst: Boolean = False; AspiPath: String = ''): Boolean;
begin
  try
    result := _InitializeASPI(InternalFirst, AspiPath);
  except
    result := False;
  end;
  if result then
  begin
    GetHostAdapterInfo(0);
    ScanDevices;
  end;
end;
var
  ModeSenseBuf1, ModeSenseBuf2: array[0..$200-1] of byte;
function  CheckWriteMethod(wm: TWriteMethod): Boolean;
var
  s, i: Integer;
begin
  Result := False;
  fillchar(ModeSenseBuf1, sizeof(ModeSenseBuf1), 0);
  fillchar(ModeSenseBuf2, sizeof(ModeSenseBuf2), 0);
  if ModeSense10($05, ModeSenseBuf1, SizeOf(ModeSenseBuf1)) then
  begin
    if ModeSenseBuf1[16] = $05 then s := 16
    else if ModeSenseBuf1[8] = $05 then s := 8
    else s := 8;
    for i :=0 to SizeOf(ModeSenseBuf1)-s-1 do ModeSenseBuf2[i] := ModeSenseBuf1[i+s];
    move(ModeSenseBuf2, ModePage05, sizeof(ModePage05));
    ModePage05.DBType := $00;
    if wm = wmTAO then
      ModePage05.WriteType := $01
    else if wm = wmSAO then
      ModePage05.WriteType := $02
    else
    begin
      ModePage05.DBType := 1;
      ModePage05.WriteType := $03;
    end;
    ModePage05.TrackMode := $10;
    ModePage05.SessionFormat := $0;
    ModePage05.Res9 := $0;
    ModePage05.PauseLen := $9600; 
    ModePage05.TrackMode := $0;
    move(ModePage05, ModeSenseBuf2, ModePage05.PageLen+s+1);
    fillchar(ModeSenseBuf1, SizeOf(ModeSenseBuf1), 0);
    for i :=0 to $3c-s do ModeSenseBuf1[i+8] := ModeSenseBuf2[i];
    if ModeSelect10(@ModeSenseBuf1[0], ModePage05.PageLen+10) then
      Result := True;
  end;
end;
function  GetWriteMethods: Boolean;
begin
  fWriteMethods := [];
  if CheckWriteMethod(wmDAO) then Include(fWriteMethods, wmDAO);
  if CheckWriteMethod(wmSAO) then Include(fWriteMethods, wmSAO);
  if CheckWriteMethod(wmTAO) then Include(fWriteMethods, wmTAO);
  result := True;

end;
function  GetDriveLetter: Char;
var
  fh: THandle;
  buf: array[0..63] of char;
  tmp: String;
  pscsiAddr: PSCSI_ADDRESS;
  returned: Cardinal;
  DriveLetter: Byte;
begin
  result := #0;
  if ASPILayerName = 'BMASPI32' then
  begin
    result := GetDriveLetterBMASPI(fHAID, fTarget, fLUN);
    exit;
  end;
  for DriveLetter := 2 to 26 do
  begin
    tmp := '\\.\'+Chr(DriveLetter+64)+':'+#0;
    fh := CreateFile (@tmp[1], GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
    if ( fh = INVALID_HANDLE_VALUE ) then
      fh := CreateFile(@tmp[1], GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    if ( fh <> INVALID_HANDLE_VALUE ) then
    begin
      FillChar(buf, Sizeof(buf), 0);
      pscsiAddr := @buf;
      pscsiAddr.Length := sizeof(SCSI_ADDRESS);
      if ( DeviceIoControl( fh, 266264, nil, 0, pscsiAddr, sizeof(SCSI_ADDRESS), returned, nil ) ) then
      begin
        if (pscsiAddr.PortNumber = fHAID) and (pscsiAddr.TargetId = fTarget) then
        begin
          result := Chr(DriveLetter+64);
          CloseHandle(fh);
          exit;
        end;
      end;
      CloseHandle(fh);
    end;
  end;
  DriveLetter := GetDiskLetter(fHAID, fTarget, fLUN);
  if DriveLetter <> 128 then
    Result := Chr(DriveLetter+65);
end;
procedure  SetDriveLetter(Value: Char);
begin
  SetDeviceByDriveLetter(Value);
end;
var
  xdi: TSRB_GetDiskInfo;

function  SetDeviceByDriveLetter(DriveLetter: Char): Boolean;
var
  fh: THandle;
  buf: array[0..63] of char;
  tmp: String;
  pscsiAddr: PSCSI_ADDRESS;
  returned: Cardinal;
  i, t, l, ha: Integer;
begin
  result := False;
  tmp := '\\.\'+DriveLetter+':'+#0;
  fh := CreateFile (@tmp[1], GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
  if ( fh = INVALID_HANDLE_VALUE ) then
    fh := CreateFile(@tmp[1], GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
  if ( fh = INVALID_HANDLE_VALUE ) then
  begin
    i := Byte(DriveLetter)-64;
    if fDriveLetters[i] <> '' then
    begin
      fdevice := fDriveLetters[i];
      CloseHandle(fh);
      Result := True;
    end;    
    for ha := 0 to fAdapters-1 do
    begin
      for t:= 0 to 7 do
      for l := 0 to 7 do
      begin
        fillchar(xdi, sizeof(xdi), 0);
        xdi.Command := SC_GET_DISK_INFO;
        xdi.HaID := ha;
        xdi.Target := t;
        xdi.Lun := l;
        SendASPI32Command (@xdi);
        if xdi.Status = 1 then
        begin
          if xdi.Int13HDriveInfo = Ord(DriveLetter) - 65 then
          begin
            tmp := IntToStr(ha)+':'+IntToStr(t)+':'+IntToStr(l)+' '+DriveLetter+':';
            fDevice := tmp;
            result := True;
            exit;
          end;
        end;
      end;
    end;
  end;
  FillChar(buf, Sizeof(buf), 0);
  pscsiAddr := @buf;
  pscsiAddr.Length := sizeof(SCSI_ADDRESS);
  if ( DeviceIoControl( fh, 266264, nil, 0, pscsiAddr, sizeof(SCSI_ADDRESS), returned, nil ) ) then
  begin
    tmp := IntToStr(pscsiAddr.PortNumber)+':'+IntToStr(pscsiAddr.TargetId)+':'+IntToStr(pscsiAddr.Lun)+' '+DriveLetter+':';
    fDevice := tmp;
    result := True;
  end;
end;
function  SelectDevice(sDevice: String): Boolean;
begin
  fDevice := sDevice;
  Result := True;
end;

function  GetDiskLetter(H, T, L: Byte): Byte;
begin
  fillchar(xdi, sizeof(xdi), 0);
  xdi.Command := SC_GET_DISK_INFO;
  xdi.HaID := H;
  xdi.Target := T;
  xdi.Lun := L;
  SendASPI32Command (@xdi);
  if xdi.Status = 1 then
    result := xdi.Int13HDriveInfo
  else
    result := 128;
end;

function  GetASPIInitialized: Boolean;
begin
  result := ASPIDLLLoaded;
end;
function  MaxWriteSpeed: Word;
begin
  if GetDeviceCapabilities then
    result := ModePage2A.MaxWriteSpeed
  else
   result := 0;
end;

procedure  SetWriteSpeed;
begin
  SetCDSpeed(fReadSpeed, Value);
  FWriteSpeed := Value;
end;

function  MaxReadSpeed: Word;
begin
  if GetDeviceCapabilities then
  begin
    result := ModePage2A.MaxReadSpeed;
  end
  else
   result := 0;
end;

function  CurrentReadSpeed;
begin
  if GetDeviceCapabilities then
    result := ModePage2A.CurReadSpeed
  else
    result := 0;
end;

function  CurrentWriteSpeed: Word;
begin
  if GetDeviceCapabilities then
    result := ModePage2A.CurWriteSpeed
  else
   result := 0;
end;

procedure  ScanDevices;
var
  ha, t, l: Integer;
  llun, lha, lt: Integer;
  str: String;
  sDrvLtr: String;
begin
  fDevices.Clear;
  lha := fHaID;
  lt := fTarget;
  llun := fLun;
  fLun := 0;
  for ha := 0 to fAdapters-1 do
  begin
    GetHostAdapterInfo(ha);
    str := fHAInquiry.HA_Identifier;
    str := trim(str);
    for t:= 0 to 7 do
    for l := 0 to 7 do
    begin
      fHaID := ha;
      fTarget := t;
      fLUN := l;
      FillChar(fInquiryData, SizeOf(fInquiryData), 0);
      if Inquiry then
      begin
        if (fInquiryData.PeripheralData = 5) or (fInquiryData.PeripheralData = 4) then
        begin
          sDrvLtr := GetDriveLetter;
          if sDrvLtr = #0 then
            sDrvLtr := '?:'
          else
            sDrvLtr := sDrvLtr+':';
          Str := IntToStr(ha)+':'+IntToStr(t)+':'+IntToStr(l)+','+sDrvLtr+' '+fInquiryData.VendorID+''+fInquiryData.ProductID+''+fInquiryData.ProductRev;
          if fInquiryData.PeripheralData = 4 then
            Str := Str + ' (not supported)';
          fDevices.Add(str);
        end;
      end;
    end;
  end;
  fHaID := lha;
  fTarget := lt;
  fLUN := llun;
  fErrorString := '';
end;

function  GetDiskInformation: Boolean;
var
  lsrb: TSRB_GetDiskInfo;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Command := SC_GET_DISK_INFO;
  lsrb.HaId := fHaID;
  lsrb.Target := fTarget;
  SendASPI32Command (@lsrb);
  result := lsrb.Status = SS_COMP;
end;

function  FormatUnit(FormatCode: Byte; Buffer: PChar; Length: Integer): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $50;
  lsrb.CDBLen := $6;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := Length;
  lsrb.BufPointer := @Buffer[0];
  lsrb.CDBCmd := $04;
  lsrb.CDBByte[1] := FormatCode;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

procedure  Rewind;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := SRB_EVENT_NOTIFY;
  lsrb.CDBLen := 06;
  lsrb.CDBCmd := $01;
  lsrb.SenseLen := SENSE_LEN;
  ExecScsiCommand(lsrb, 10000);
end;

function  GetHostAdapterInfo;
begin
  fillchar(fHAInquiry, sizeof(fHAInquiry), 0);
  fHAInquiry.Command := SC_HA_INQUIRY;
  fHAInquiry.HaId := ha;
  SendASPI32Command (@fHAInquiry);
  if fHAInquiry.Status =1 then
    result := fHAInquiry.HA_Count
  else
    result := -1;
  fAdapters := result;
  if fAdapters = 0 then fAdapters := 7;
end;

function  AbortSCSICommand;
begin
  fillchar(fSCAbort, sizeof(fSCAbort), 0);
  fSCAbort.Command := SC_ABORT_SRB;
  fSCAbort.HaID := fHaID;
  fSCAbort.ToAbort := @srb;
  result := fSCAbort.Status = 1;
end;

procedure  SetDeviceID;
var
  P: String;
  v: String;
begin
  if Length(Value) < 5 then
  begin
    if Value <> '' then
    fHaID := 254;
    fTarget := 254;
    fLun := 254;
    fDevice := '';
    fDeviceName := '';
    fDeviceType := 127;
  end
  else
  begin
    try
      fHaID := StrToInt(Value[1]);
      fTarget := StrToInt(Value[3]);
      fLun := StrToInt(Value[5]);
      fDevice := Value;
      DeviceChanged;
      if (fInquiryData.PeripheralData = 5) or (fInquiryData.PeripheralData = 4) then
      begin
        p := fInquiryData.ProductID;
        p := Trim(p);
        v := fInquiryData.VendorID;
        v := Trim(v);
        if (P = 'CD-W512EB') then
          TEAC512EB := true
        else
          TEAC512EB := False;
        if (P = 'CRX175E') or (P = 'CD-RW  CRX800E') then
          SonyPowerBurn := True
        else
          SonyPowerBurn := False;
        If length(V)<3 then
    	    V:='     ';
        if
        ((P[1] = 'U') and (P[2] = 'J') and (P[3] = 'D') and (P[4] = 'A')) or
        ((V[1] = 'T') and (V[2] = 'D') and (V[3] = 'K')) then
          Wait02 := True
        else
          Wait02 := False;
        if (P = 'CD-RW  CRX100E') then
          SonyCRX100E := True
        else
          SonyCRX100E := False;
        if (P = 'CDRW321040X') then
          fImmedCT := False
        else
          fImmedCT := True;
        if (fInquiryData.ProductID = 'DVD-RW  DVR-103 ') or (fInquiryData.ProductID = 'DVD-RW  DVR-104 ') then
        begin
          PDVR103 := True;
          fImmedCT := True;
        end
        else
          PDVR103 := False;
        if Assigned(fOnDeviceChange) then
        GetWriteMethods;
      end
      else
      begin
        fDevice := '';
        fDeviceName := '';
        fDeviceType := 127;
        fHaID := 254;
        fTarget := 254;
        fLun := 254;
      end;
      except
        fDevice := '';
        fDeviceName := '';
        fDeviceType := 127;
        fHaID := 254;
        fTarget := 254;
        fLun := 254;
      end;

  end;
end;

procedure  DeviceChanged;
var
  str: String;
  i: Integer;
begin
  GetDeviceCapabilities;
  if Inquiry then
  begin
    fDeviceType := fInquiryData.PeripheralData;
    with fInquiryData do
    begin
      for i:= 8 to 15 do if VendorID[i] < #32 then VendorID[i] := #32;
      for i:= 16 to 31 do if ProductID[i] < #32 then ProductID[i] := #32;
      for i:= 32 to 35 do if ProductRev[i] < #32 then ProductRev[i] := #32;
      for i:= 36 to 55 do if VendorSpecific[i] < #32 then VendorSpecific[i] := #32;
    end;

    Str := fInquiryData.VendorID+' '+fInquiryData.ProductID+' '+fInquiryData.ProductRev;
    fDeviceName := Str;
  end
  else
    fDeviceName := 'Error';
end;

function  GetAspiError(Status, HaStat, TargStat : BYTE) : String;
begin
  fTargetBusy := False;
  result := 'ERROR';
{}
end;

function  AdditionalSenseInformation(scm : TSRB_ExecSCSICmd): String;
var
  s: TSenseArea;
  ErrStr: String;
begin
  s := scm.SenseArea;
  ErrStr := '';
  {$I asc_info.inc}
  fLastSense := s;
  if ErrStr = '' then ErrStr := IntToStr(s.AddSenseCode)+','+IntToStr(s.AddSenQual);
  if s.AddSenLen <> 0 then
    fErrorString := fErrorString + ' ('+ErrStr+')';
end;

function  DecodeSense(scm : TSRB_ExecSCSICmd): String;
var
  s: TSenseArea;
begin
  s := scm.SenseArea;
  Result := GetAspiError(scm.Status, scm.HaStat, scm.TargStat);
  if (Result = ERR_CHECKCONDITION) then
    if s.ErrorCode = 0 then
      Result := ERR_NONE
    else
      if (s.ErrorCode AND $7E) <> $70 then Result := ERR_UNKNOWN
      else
  case (s.SenseKey AND $0F) of
    0:
    begin
      if (s.SenseKey AND $80) <> 0 then Result := ERR_FILEMARK
      else if (s.SenseKey AND $40) <> 0 then Result := ERR_ENDOFMEDIA
      else if (s.SenseKey AND $20) <> 0 then Result := ERR_ILLEGALLENGTH
      else if (s.SenseKey AND $80) <> 0 then Result := ERR_INCORRECTLENGTH
      else  Result := ERR_AS_00_00;
    end;
    1 : Result := ERR_RECOVEREDERROR;
    2 : Result := ERR_NOTREADY;
    3 : Result := ERR_MEDIUMERROR;
    4 : Result := ERR_HARDWAREERROR;
    5 : Result := ERR_ILLEGALREQUEST;
    6 : Result := ERR_UNITATTENTION;
    7 : Result := ERR_DATAPROTECT;
    8 : Result := ERR_ERASECHECK;
    10: Result := ERR_COPYABORTED;
    11: Result := ERR_ABORTEDCOMMAND;
    13: Result := ERR_VOLUMEOVERFLOW;
    14: Result := ERR_MISCOMPARE;
    15: Result := ERR_RESERVED;         
  end;
  fErrorString := Result;
  AdditionalSenseInformation(scm);
end;

function  ExecScsiCommand;
var
  Event: THandle;
  EventNotify: Boolean;
  S : String;
  i: integer;
begin
  fErrorString := '';
  Event := 0;
  fTargetBusy := False;
  FillChar(fLastSense, sizeof(fLastSense), 0);
  if (not GetASPIInitialized) or (fHaID = 254) then
  begin
    result := $E2;
    exit;
  end;
  EventNotify := sc.Flags and $40 = $40;
  if EventNotify then
  begin
    Event := CreateEvent(nil, true, false, nil); 
    ResetEvent(Event);
    sc.PostProc := Event;
  end
  else
    sc.PostProc := 0;
  sc.Command := SC_EXEC_SCSI_CMD;
  sc.Target := fTarget;
  sc.HaId := fHaID;
  sc.Lun := fLun;
  sc.SenseLen := SENSE_LEN;
  if Timeout <= 10000 then
    Timeout := 10000;

  sc.CDBByte[1] := ((fLun AND 7) SHL 5) OR (sc.CDBByte[1] AND $1F);
  if (sc.CDBCmd <> $2a) and (sc.CDBCmd <> $5b) and (sc.CDBCmd <> $53) then
    SendASPI32Command (@sc)
  else
    sc.Status := 1;
    SendASPI32Command (@sc);
  if EventNotify then
  begin
    if sc.status = ss_pending then
      WaitForSingleObject(Event, TimeOut);
  end;
  if (sc.Status <> 1) and (sc.Status <> 0) then
    DecodeSense(sc);  
  if GenerateLogs then
  begin
    s := IntToStr(sc.HAID)+':'+IntToHex(sc.Target, 1)+':'+IntToStr(sc.Lun)+' <'+IntToHex(sc.Status, 2)+'>'+' ['+IntToHex(sc.BufLen,5)+']';
    s := '('+IntToHex(sc.CDBCmd, 2)+') {'+IntToHex(sc.Flags, 2)+'|'+IntToHex(sc.CDBLen, 2)+'} - '+s+' ';
    for i:=1 to 15 do s := s + IntTohex(sc.CDBByte[i], 2)+' ';
    s := CommandName[sc.cdbCmd]+' '+s+' ';
    s := s + '|';
    if sc.BufLen <> 0 then
    begin
      for i:=0 to Min($100, sc.BufLen-1) do s := s + IntTohex(Byte(sc.BufPointer[i]), 2)+' ';
    end;
  end
  else
  begin
    if not DontShowError then
    begin
      DontShowError := True;
      if (sc.Status <> 1) and (sc.Status <> 0) then
      if (sc.cdbcmd <> $5C) and (sc.cdbcmd <> $12) and (sc.cdbcmd <> $35) then
      if (sc.cdbcmd <> $BB) and (sc.cdbcmd <> $5a) and (sc.cdbcmd <> $35) then
      if (sc.cdbcmd <> $B6) and (sc.cdbcmd <> $1e) and (sc.cdbcmd <> $55) then
      if (sc.cdbcmd <> $00) and (sc.cdbcmd <> $01) and (sc.cdbcmd <> $23) then
      if (sc.cdbcmd <> $43) and (sc.cdbcmd <> $04) and (sc.cdbcmd <> $46) then
      if (sc.cdbcmd <> $52) and (sc.cdbcmd <> $1B) and (sc.cdbcmd <> $51) then
      if not ((sc.SenseArea.AddSenseCode = 04) and (sc.SenseArea.AddSenQual = 08)) then
      begin
        s := IntToStr(sc.HAID)+':'+IntToHex(sc.Target, 1)+':'+IntToStr(sc.Lun)+' <'+IntToHex(sc.Status, 2)+'>'+' ['+IntToHex(sc.BufLen,5)+']';
        s := '('+IntToHex(sc.CDBCmd, 2)+') {'+IntToHex(sc.Flags, 2)+'|'+IntToHex(sc.CDBLen, 2)+'} - '+s+' ';
        for i:=1 to 15 do s := s + IntTohex(sc.CDBByte[i], 2)+' ';
        s := CommandName[sc.cdbCmd]+' '+s+' ';
        s := s + '|';
        if sc.BufLen <> 0 then
        begin
          for i:=0 to Min($100, sc.BufLen-1) do s := s + IntTohex(Byte(sc.BufPointer[i]), 2)+' ';
        end;
      end;
    end;
  end;

  if sc.Status = 0 then
  begin
    if EventNotify then
    begin
      CloseHandle(Event);
      ResetEvent(Event);
    end;
    AbortSCSICommand(sc);
    sc.Status := 4;
    sc.HaStat := HASTAT_TIMEOUT;
  end
  else
  begin
    CloseHandle(Event);
  end;
  result := 1;
end;

function  SendCueSheet(buffer: PChar; BufferLength: Integer): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $50;
  lsrb.CDBLen := $0A;
  lsrb.BufLen := bufferlength;
  lsrb.BufPointer := buffer;
  lsrb.CDBCmd := $5D;
  lsrb.CDBByte[7] := HiByte(Bufferlength);
  lsrb.CDBByte[8] := LoByte(Bufferlength);
  ExecScsiCommand(lsrb, 30000);
  result := lsrb.Status = SS_COMP;
end;

function  TestUnitReady;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := SRB_EVENT_NOTIFY;
  lsrb.CDBLen := 06;
  lsrb.SenseLen := SENSE_LEN;
  ExecScsiCommand(lsrb, TimeOut);
  result := lsrb.Status = SS_COMP;
end;

function  WaitForReady(TimeOut: Integer = 10000; Step: Integer = 5000): Boolean;
var
  OTO: Integer;
label Wait02End;
begin
  result := false;
  OTO := TimeOut;
  fillchar(_SenseInfo, sizeof(_SenseInfo), 0);
  if (PDVR103) or (Wait02) then
  begin
    repeat
      SleepEx(1500, False);
      Dec(OTO, 1500);
      if OTO < 0 then goto Wait02End;
      TestUnitReady;
    until (fLastSense.AddSenseCode <> 4);
  end;
Wait02End:  
  if (MediumIs <> mtDVD_RAM) then
  begin
    repeat
      SleepEx(Step, False);
      ReadDiscInformation;
      Dec(OTO, Step);
      if OTO < 0 then exit;
    until (fLastSense.AddSenseCode <> 4);
  end
  else
  _SenseInfo.ASC := 4;
  if not TestUnitReady or (_SenseInfo.ASC <> 0) then
  begin
    repeat
      SleepEx(Step, False);
      Dec(OTO, Step);
      if OTO < 0 then exit;
      TestUnitReady;
    until (fLastSense.AddSenseCode <> 4);
    repeat
      fillchar(_SenseInfo, sizeof(_SenseInfo), 0);
      RequestSense(@_SenseInfo, sizeof(_SenseInfo));
      Dec(OTO, Step);
      if OTO < 0 then exit;
      if _SenseInfo.ResponseCode = $F0 then
        _SenseInfo.ASC := 04;
      if _SenseInfo.ASC = 4 then
        SleepEx(Step, False);
    until (_SenseInfo.ASC = 0);
  end;
  result := true;
end;

function  ScsiReset: Integer;
var
  srb: SRB_BusDeviceReset;
  res: Integer;
begin
  fillchar (srb, sizeof(SRB_BusDeviceReset), 0);
  srb.Target := fTarget;
  srb.Command := SC_RESET_DEV;
  srb.Flags := SRB_DIR_SCSI;
  srb.Lun := fLun;
  srb.HaId := fHaID;
  res := SendASPI32Command (@srb);
  result := res;
end;

var
  th: TEraseThread;

function  EraseProgress: Integer;
begin
  if not fErasing then
    result := 0
  else
    result := th.Progress;
end;

procedure TEraseThread.EraseDoneEvent;
begin
  fErasing := False;
end;

procedure TEraseThread.Execute;
var
  Seconds: Double;
  i: Integer;
label endoferase;  
begin
  fProgress := 0;
  Seconds := 0;
  if  (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) then
  begin
    repeat
      fProgress  := Round(Seconds / AproxSecs * 100);
      Seconds := Seconds + 1;
      if not TestUnitReady then
        SleepEx(2000, False);
      fProgress  := Round(Seconds / AproxSecs * 100);
    until (fLastSense.AddSenseCode <> 4) or (AproxSecs < Seconds);
    SleepEx(5000, False);
    repeat
      fProgress  := Round(Seconds / AproxSecs * 100);
      Seconds := Seconds + 1;
      if not TestUnitReady then
        SleepEx(2000, False);
    until (fLastSense.AddSenseCode <> 4) or (AproxSecs < Seconds);
    
  end
  else if ((MediumIs = mtDVD_PLUSRW)) then
  begin
    buf1 := #$00#$82#$00#$08#$00#$23#$05#$40#$98#$00#$00#$00;
    repeat
       Sleep(2000);
       Seconds := Seconds + 2;
       ReadDiscInformation;
       fProgress  := Round(Seconds / AproxSecs * 100);
    until (fLastSense.AddSenseCode <> 04);

    fillchar(buf2, $ff00, 0);
    if (EraseType_ = etQuick) then
    begin
      for i:=0 to 64 do
        if not Write10(i*32, 32, buf2, 32*2048) then
          goto endoferase;
        SleepEx(300, False);
        fProgress  := Round(Seconds / AproxSecs * 100);
        Seconds := Seconds + 1;
    end
    else
    begin
      for i:=0 to 71000 do
        if not Write10(i*32, 32, buf2, 32*2048) then
          goto endoferase;
        SleepEx(300, False);
        Seconds := Seconds + 1;
        fProgress  := Round(Seconds / AproxSecs * 100);
    end;
    FlushCache(30000, FALSE);
    CloseTrackDVD(FALSE, 03, 0, 0, 0);

  end else if ((MediumIs = mtDVD_RAM))and (EraseType_ = etQuick) then
  begin
    fillchar(buf2, $ff00, 0);
    for i:=0 to 64 do
    begin
      Write10(i*32, 32, buf2, 32*2048);
    end;
    FlushCache(30000, FALSE);
  end;
endoferase:
  if Wait02 then
  begin
    repeat
      fProgress  := Round(Seconds / AproxSecs * 100);
      Seconds := Seconds + 2;
      if not TestUnitReady then
        SleepEx(2000, False);
    until (fLastSense.AddSenseCode <> 4) or (AproxSecs < Seconds);
    ReadDiscInformation;
    SleepEx(5000, False);
  end;
  if not TestUnitReady or not ReadDiscInformation then
  begin
    if (Medium <> mtDVD_RAM) then
    begin
      repeat
        fProgress  := Round(Seconds / AproxSecs * 100);
        ReadDiscInformation;
        Seconds := Seconds + 5;
        if fLastSense.AddSenseCode = 4 then
          SleepEx(5000, False);
      until ( fLastSense.AddSenseCode <> 4) or (AproxSecs < Seconds);
    end;
    repeat
      fProgress  := Round(Seconds / AproxSecs * 100);
      Seconds := Seconds + 2;
      if not  TestUnitReady then
        SleepEx(2000, False);
    until ( fLastSense.AddSenseCode <> 4) or (AproxSecs < Seconds);

    repeat
      fProgress  := Round(Seconds / AproxSecs * 100);
      fillchar(_SenseInfo, sizeof(_SenseInfo), 0);
       RequestSense(@_SenseInfo, sizeof(_SenseInfo));
      Seconds := Seconds + 2;
      if _SenseInfo.ResponseCode = $F0 then
        _SenseInfo.ASC := 04;
      if _SenseInfo.ASC = 4 then
        SleepEx(2000, False)
      else
        SleepEx(2000, False);
    until (_SenseInfo.ASC = 0) or (AproxSecs < Seconds);
  end;

  if ASPILayerName = 'BMASPI32' then
    _ReInitializeASPI;
  fProgress := 100;
   SetWriteParams(False, False, False, 1);
   SetCDSpeed( MaxReadSpeed, 2);
   Rewind;
   LockMedium(True);
   LoadMedium;
   Rewind;
   TestUnitReady;
   fErasing := False;
  if  ConsoleApplication then
  begin

  end
  else
    Synchronize(EraseDoneEvent);
end;

function  EraseDisc;
var
  lsrb: TSRB_ExecSCSICmd;
  WriteSpeed, i: Integer;
begin
  TestUnitReady;
  LoadMedium;
  MediumIs := DiscType;
  Result := False;
  EraseType_ := EraseType;
  if not TestUnitReady then
  begin
    if not waitForReady(30000) then
    begin
      result := False;
      exit;
    end;
  end;
  if (MediumIs <> mtDVD_RAM) then
  begin
    SetCDSpeed(fReadSpeed, fWriteSpeed);
    SetWriteParams(False, False, False, MediumIs);
  end;
  WriteSpeed := CurrentWriteSpeed;
  if not TestUnitReady then
  begin
    if not waitForReady(30000) then
    begin
      result := False;
      exit;
    end;
  end;
  if (MediumIs = mtDVD_RAM) or (MediumIs = mtDVD_RW) or (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) or (MediumIs = mtDVD_PLUSRW) then
  else
  if (MediumIs <> mtDVD_RAM) and (MediumIs <> mtDVD_PLUSRW) then
  begin
    fillchar(lsrb, sizeof(lsrb), 0);
    lsrb.Flags := SRB_EVENT_NOTIFY;
    lsrb.CDBLen := $0C;
    lsrb.SenseLen := SENSE_LEN;
    lsrb.CDBCmd := $A1;
    lsrb.CDBByte[1] := Byte(EraseType);
    lsrb.CDBByte[1] := lsrb.CDBByte[1] or 16;
    ExecScsiCommand(lsrb, (60 * 1000)*80);
    result := (lsrb.Status = SS_COMP) or (lsrb.Status = SS_PENDING);
  end
  else
  begin
    if (MediumIs = mtDVD_RAM) then
    begin
      if EraseType = etQuick then
      begin
        fillchar(buf2, $ff00, 0);
        for i:= 0 to 10 do
        begin
          Write10(i*32, 32, buf2, 32*2048);
          result := True;
        end;
        FlushCache(30000, FALSE);
      end
      else
      begin
        Buf1 := #$00#$82#$00#$08#$00#$22#$21#$20#$00#$00#$08#$00;
        result := FormatUnit($11, Buf1, 12);
      end;
    end
    else if (MediumIs = mtDVD_PLUSRW) then
    begin
      buf1 := #$00#$82#$00#$08#$00#$23#$05#$40#$98#$00#$00#$00;
      result := FormatUnit($11, @buf1, 12);
      if (not result) then
      begin
        Rewind;
        Sleep(1000);
        result := FormatUnit($11, @buf1, 12);
      end;

    end;
  end;
  if result then
  begin
    th := TEraseThread.Create(True);
    th.Medium := MediumIs;
    th.FreeOnTerminate := True;
    if MediumIs < 4 then
    begin
      th.AproxSecs := 75 * 60;
      if EraseType = etQuick then
        th.AproxSecs := 150;
      if EraseType = etComplete then
        th.AproxSecs := 75 * 60;
    end
    else
    begin
      th.AproxSecs := 74 * 60 * 2;
      if EraseType = etQuick then
      begin
        th.AproxSecs := 600;
        if MediumIs = mtDVD_RW then
          th.AproxSecs := 60 * 3;
      end;
      if EraseType = etComplete then
        th.AproxSecs := 74 * 60;
    end;
    fErasing := True;
    th.Resume;
  end
  else
  begin
    fErasing := False;
    if Assigned(fEraseDone) then
      fEraseDone(nil,True);
  end;

end;

function  LockMedium(Unlock: Boolean = False): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  if Unlock then
    CloseFH := True;
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := SRB_EVENT_NOTIFY;
  lsrb.CDBLen := $06;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $1E;
  if not Unlock then
    lsrb.CDBByte[4] := 1;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
  CloseFH := False;
end;

procedure  LockDrive(UnLock: Boolean = True);
begin
  SPTI.CloseFH := UnLock;
end;

function  Inquiry: Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $06;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := 128;
  lsrb.BufPointer := @fInquiryData;
  lsrb.CDBCmd := $12;
  lsrb.CDBByte[4] := 128;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  ModeSense10;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  fillchar(buf, sizeof(buf), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := buflen;
  lsrb.BufPointer := @buf2;
  lsrb.CDBCmd := $5A;
  if ps then
    lsrb.CDBByte[2] := PageCode + 128
  else
    lsrb.CDBByte[2] := PageCode;
  lsrb.CDBByte[7] := HiByte(BufLen);
  lsrb.CDBByte[8] := LoByte(BufLen);
  ExecScsiCommand(lsrb, 20000);
  result := lsrb.Status = SS_COMP;
  if result then
  begin
    move(buf2, buf, buflen);
  end;
end;

function  GetConfigData(RT: Byte; Profile: Word; buf: Pointer; buflen: Integer): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := buflen;
  lsrb.BufPointer := buf;
  lsrb.CDBCmd := $46;
  lsrb.CDBByte[1] := RT;
  lsrb.CDBByte[3] := Profile;
  lsrb.CDBByte[8] := buflen;
  ExecScsiCommand(lsrb, 30000);
  result := lsrb.Status = SS_COMP;
end;

function  SetStreaming(Buffer: Pointer; BufLength: Word): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $50;
  lsrb.CDBLen := 12;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := BufLength;
  lsrb.BufPointer := Buffer;
  lsrb.CDBCmd := $B6;
  CvtEndians(BufLength, lsrb.CDBByte[9], 2);
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  SendDVDStructure(Format: Byte; Buf: Pointer; BufLength: Word): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $50;
  lsrb.CDBLen := 12;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := BufLength;
  lsrb.BufPointer := buf;
  lsrb.CDBCmd := $BF;
  lsrb.CDBByte[7] := Format;
  CvtEndians(BufLength, lsrb.CDBByte[8], 2);
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  SendDVDStructureTimeStamp(Time: TDateTime): Boolean;
var
  str: String;
begin
  str := formatDateTime('yyyymmddhhnnss', Time);
  buf1 := #$00+#$14+#$00+#$00+#$00+#$00+#$00+#$00;
  move(str[1], buf1[8], 14);
  result := SendDVDStructure($0f, @buf1, $16);
end;

function  ReadDVDStructure(Layer, Format: Byte; Buf: Pointer; BufLength: Word): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0C;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := BufLength;
  lsrb.BufPointer := buf;
  lsrb.CDBCmd := $AD;
  lsrb.CDBByte[7] := Format;
  lsrb.CDBByte[6] := Layer;
  CvtEndians(BufLength, lsrb.CDBByte[8], 2);
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  ModeSelect10;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  move(Buffer[0], buf2[0], buflen);
  lsrb.Flags := $50;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := buflen;
  lsrb.BufPointer := Buf2;
  lsrb.CDBCmd := $55;
  lsrb.CDBByte[1] := 16;
  lsrb.CDBByte[8] := buflen; 
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  IsJustLinkCapable: Boolean;
var
  page: Byte;
  s: Integer;
begin
  page := $30;
  result := false;
  fillchar(buf3, $80, 0);
  if ModeSense10(page, buf3, $80, False) then
  begin
    if buf3[16] = page then s := 16
    else if buf3[8] = page then s := 8
    else if buf3[7] = page then s := 7
    else s := 0;
    if s <> 0 then
      result := true;
  end;
end;

function  GetDeviceCapabilities;
var
  i: Integer;
  s: Byte;
  page: integer;
begin
  fDevCaps := [];
  s := 15;
  page := $2a;
  fDeviceMaxWriteSpeed := 0;
  fDeviceMaxReadSpeed := 0;
  fillchar(ModePage2A , sizeof(ModePage2A), 0);
  fillchar(buf3, 255, 0);
  if ModeSense10(page, buf3, $FF, True) then
  begin
    if buf3[16] = page then s := 16
    else if buf3[8] = page then s := 8
    else if buf3[7] = page then s := 7
    else s := 8;
    for i :=0 to 254-s do buf3[i] := buf3[i+s];
    move(buf3, ModePage2A, sizeof(ModePage2A));
    if (ModePage2A.DiscReadCaps  and $01 = $01) then Include(fDevCaps, dcReadCDR);
    if (ModePage2A.DiscReadCaps  and $02 = $02) then Include(fDevCaps, dcReadCDRW);
    if (ModePage2A.DiscReadCaps  and $04 = $04) then Include(fDevCaps, dcReadMethod2);
    if (ModePage2A.DiscReadCaps  and $08 = $08) then Include(fDevCaps, dcReadDVD);
    if (ModePage2A.DiscReadCaps  and $10 = $10) then begin Include(fDevCaps, dcReadDVDR); Include(fDevCaps, dcReadDVDRW); end;
    if (ModePage2A.DiscReadCaps  and $20 = $20) then Include(fDevCaps, dcReadDVDRAM);

    if (ModePage2A.DiscWriteCaps and $01 = $01) then Include(fDevCaps, dcWriteCDR);
    if (ModePage2A.DiscWriteCaps and $02 = $02) then Include(fDevCaps, dcWriteCDRW);
    if (ModePage2A.DiscWriteCaps and $04 = $04) then Include(fDevCaps, dcWriteTest);
    if (ModePage2A.DiscWriteCaps and $10 = $10) then begin Include(fDevCaps, dcWriteDVDR); Include(fDevCaps, dcWriteDVDRW); end;
    if (ModePage2A.DiscWriteCaps and $20 = $20) then Include(fDevCaps, dcWriteDVDRAM);
    if (ModePage2A.AudioPlay     and $80 = $80) then Include(fDevCaps, dcUnderrunProtection);
    if IsJustLinkCapable or TEAC512EB then
      Include(fDevCaps, dcUnderrunProtection);
    if ModePage2A.MaxWriteSpeed = 0 then
    begin
      if (ModePage2A.DiscWriteCaps and $20 = $20) then
        fDeviceMaxWriteSpeed := 1;
    end
    else
      fDeviceMaxWriteSpeed := L2MW(ModePage2A.MaxWriteSpeed);

    if ModePage2A.MaxReadSpeed = 0 then
    begin
      if (ModePage2A.DiscWriteCaps and $20 = $20) then
        fDeviceMaxReadSpeed := 1;
    end
    else
      fDeviceMaxReadSpeed := L2MW(ModePage2A.MaxReadSpeed);
      if GetConfigData(2, $2a, @dtBuf, sizeof(dtBuf)) then
      begin
        dtBuf.CurrentProfile := L2MW(dtBuf.CurrentProfile);
        dtBuf.FeatureCode := L2MW(dtBuf.FeatureCode);
        if (dtBuf.FeatureCode = $2a) then
        begin
          Include(fDevCaps, dcReadDVDPLUSR);
          Include(fDevCaps, dcReadDVDPLUSRW);
          if dtBuf.OtherData[0] = 1 then
          begin
            Include(fDevCaps, dcWriteDVDPLUSR);
            Include(fDevCaps, dcWriteDVDPLUSRW);
          end;
        end;
      end;
      if GetConfigData(2, $2f, @dtBuf, sizeof(dtBuf)) then
      begin
        dtBuf.CurrentProfile := L2MW(dtBuf.CurrentProfile);
        dtBuf.FeatureCode := L2MW(dtBuf.FeatureCode);
        if (dtBuf.FeatureCode = $2f) then
        begin
          Include(fDevCaps, dcReadDVDR);
          Include(fDevCaps, dcReadDVDRW);
          if dtBuf.OtherData[0] = 1 then
          begin
            Include(fDevCaps, dcWriteDVDR);
            Include(fDevCaps, dcWriteDVDRW);
          end;
        end;
      end;

  end
  else
  begin
    fDeviceMaxWriteSpeed := 0;
    fDeviceMaxReadSpeed := 0;
  end;
  fillchar(buf3, 255, 0);
  if ModeSense10(page, buf3, $80) then
  begin
    if buf3[16] = page then s := 16
    else if buf3[8] = page then s := 8
    else if buf3[7] = page then s := 7;
    for i :=0 to 254-s do buf3[i] := buf3[i+s];
    move(buf3, ModePage2A, sizeof(ModePage2A));
    if ModePage2A.CurWriteSpeed = 0 then
      ModePage2A.CurWriteSpeed := 1385;
    if ModePage2A.MaxWriteSpeed = 0 then
      ModePage2A.MaxWriteSpeed := 1385;
    ModePage2A.CurWriteSpeed := L2MW(ModePage2A.CurWriteSpeed);
    ModePage2A.MaxWriteSpeed := L2MW(ModePage2A.MaxWriteSpeed);

    ModePage2A.CurReadSpeed := L2MW(ModePage2A.CurReadSpeed);
    ModePage2A.MaxReadSpeed := L2MW(ModePage2A.MaxReadSpeed);
    ModePage2A.BufferSize := L2MW(ModePage2A.BufferSize);
    fillchar(buf3, 255, 0);
    Result := True;
    if fDeviceMaxWriteSpeed < ModePage2A.MaxWriteSpeed then
      fDeviceMaxWriteSpeed := ModePage2A.MaxWriteSpeed;
    if fDeviceMaxReadSpeed < ModePage2A.MaxReadSpeed then
      fDeviceMaxReadSpeed := ModePage2A.MaxReadSpeed;

  end
  else
    Result := False;
end;

function  SetWriteParams(TestWrite, UnderrunProtected, MultiSession: Boolean; MediumIs: SmallInt): Boolean;
var
  s, i: Integer;
begin
  if MediumIs = mtDVD_RAM then
  begin
    result := true;
    exit;
  end;
  Result := False;
  fillchar(ModeSenseBuf1, sizeof(ModeSenseBuf1), 0);
  fillchar(ModeSenseBuf2, sizeof(ModeSenseBuf2), 0);
  if ModeSense10($05, ModeSenseBuf1, SizeOf(ModeSenseBuf1)) then
  begin
    if ModeSenseBuf1[16] = $05 then s := 16
    else if ModeSenseBuf1[8] = $05 then s := 8
    else if ModeSenseBuf1[7] = $05 then s := 7 else s := 8;
    for i :=0 to SizeOf(ModeSenseBuf1)-s-1 do ModeSenseBuf2[i] := ModeSenseBuf1[i+s];
    move(ModeSenseBuf2, ModePage05, sizeof(ModePage05));
    ModePage05.PageLen := $32;
    if (MediumIs = mtDVD_R) then
    begin
      ModePage05.WriteType := $60;
      ModePage05.TrackMode := $e5;
      ModePage05.LinkSize := $10;
      ModePage05.PacketSize := $10000000;
    end
    else
    begin
      ModePage05.WriteType := 1;
      ModePage05.TrackMode := 4;
      ModePage05.LinkSize := 0;
      ModePage05.PacketSize := 0;
    end;
    ModePage05.Dbtype := 8;

    if fwms then
    begin
      ModePage05.WriteType := 02;
      ModePage05.dbtype := 0;
    end;
    if (MediumIs = mtDVD_RW) and (MediumIs = mtDVD_RW_RO) then
    begin
      ModePage05.WriteType := $20;
      ModePage05.TrackMode := $05;
    end
    else if (MediumIs = mtDVD_PLUSR) or (MediumIs = mtDVD_PLUSRW) then
    begin
      ModePage05.WriteType := $22;
      ModePage05.TrackMode := ModePage05.TrackMode or $c0
    end;

    if SonyPowerBurn then
      ModePage05.WriteType := ModePage05.WriteType or $20;
    if (MediumIs <> mtDVD_R) and (MediumIs <> mtDVD_RW) and (MediumIs <> mtDVD_RW_RO) and (MediumIs <> mtDVD_RW_SR) and (MediumIs <> mtDVD_PLUSR) then
    begin
      if not TestWrite then
      begin
        if not fFinalizeDisc then
          ModePage05.TrackMode := ModePage05.TrackMode or $c0
        else
          if ModePage05.TrackMode and $c0 = $c0 then ModePage05.TrackMode := ModePage05.TrackMode - $c0;
      end
      else
        if ModePage05.TrackMode and $c0 = $c0 then ModePage05.TrackMode := ModePage05.TrackMode - $c0;
    end
    else
    begin
      ModePage05.TrackMode := ModePage05.TrackMode or $E0;
    end;
    if (dcUnderrunProtection in fDevCaps) and (UnderrunProtected) then
      ModePage05.WriteType := ModePage05.WriteType or $40
    else
      if ModePage05.WriteType and $40 = $40 then ModePage05.WriteType := ModePage05.WriteType - $40;
    if (dcWriteTest in fDevCaps) and (TestWrite) then
      ModePage05.WriteType := ModePage05.WriteType or $10
    else
      if ModePage05.WriteType and $10 = $10 then ModePage05.WriteType := ModePage05.WriteType - $10;
    ModePage05.SessionFormat := 0;

    ModePage05.Res6 := 0;
    ModePage05.Res9 := 0;
    ModePage05.Host_App_Code := 0;
    if (MediumIs = mtDVD_PLUSR) then
    begin
      ModePage05.Res6 := $10;
      ModePage05.PacketSize := $10000000;
    end;

    if (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) then
    begin
      ModePage05.TrackMode := $4;
      ModePage05.WriteType := $20;
      ModePage05.TrackMode := $c5;
      ModePage05.LinkSize := $10;
    end;

    if (MediumIs = mtDVD_PLUSR) then
    begin
      ModePage05.TrackMode := $c4;
      ModePage05.Res6 := $0;
    end;
    if (MediumIs = mtDVD_PLUSRW) then
    begin
      ModePage05.TrackMode := $4;
      ModePage05.LinkSize := $10;
    end;

    ModePage05.PauseLen := $9600;
    move(ModePage05, ModeSenseBuf2, ModePage05.PageLen+s+1);
    fillchar(ModeSenseBuf1, 64, 0);
    for i :=0 to $3c-s do ModeSenseBuf1[i+8] := ModeSenseBuf2[i];
    if ModeSelect10(@ModeSenseBuf1[0], ModePage05.PageLen+10) then
      Result := True;
  end;
end;

function  SendOPC;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $40;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $54;
  lsrb.CDBByte[1] := $01;
  ExecScsiCommand(lsrb, 150000);
  result := lsrb.Status = SS_COMP;
end;

function  CloseTrack(Session: Boolean=False; Immed: Boolean = True; DVDPR: Boolean = False; TrackNo: Byte = 0): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Command := 2;
  lsrb.Flags := $40;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $5B;
  if Immed then
    lsrb.CDBByte[1] := $01;
  if DVDPR then
  begin
    lsrb.CDBByte[2] := $05;
  end
  else
  begin
    if Session then
      lsrb.CDBByte[2] := $01
    else
      lsrb.CDBByte[2] := $02;
  end;
  lsrb.CDBByte[4] := $00;
  if Session then
    lsrb.CDBByte[5] := $ff;
  ExecScsiCommand(lsrb, 180000);
  result := lsrb.Status = SS_COMP;
end;

function  CloseTrackDVD(Immed: Boolean; b2, b3, b4, b5: Byte): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Command := 2;
  lsrb.Flags := $40;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $5B;
  if Immed then
    lsrb.CDBByte[1] := $01;
  lsrb.CDBByte[2] := b2;
  lsrb.CDBByte[3] := b3;
  lsrb.CDBByte[4] := b4;
  lsrb.CDBByte[5] := b5;
  ExecScsiCommand(lsrb, 180000);
  result := lsrb.Status = SS_COMP;
end;

function  LoadMedium(Eject: Boolean=False): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  CloseFH := True;
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Command := 2;
  lsrb.Flags := $40;
  lsrb.CDBLen := $06;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $1B;
  lsrb.CDBByte[1] := $00;
  if Eject then
    lsrb.CDBByte[4] := $02
  else
    lsrb.CDBByte[4] := $03;
  ExecScsiCommand(lsrb, 20000);
  result := lsrb.Status = SS_COMP;
  CloseFH := False;
end;

function  Read10(LBA: Cardinal; TransferLength: Word; Buffer: PChar; buflen: Cardinal): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := BufLen;
  lsrb.BufPointer := Buffer;
  lsrb.CDBCmd := $28;
  CvtEndians(LBA, lsrb.CDBByte[2], 4);
  lsrb.CDBByte[7] := HiByte(TransferLength);
  lsrb.CDBByte[8] := LoByte(TransferLength);
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  ReadCD(LBA: Cardinal; TransferLength: Word; Buffer: PChar; buflen: Cardinal; SecType, Other, SubChannelsSelection: Byte): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0C;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := BufLen;
  lsrb.BufPointer := Buffer;
  lsrb.CDBCmd := $BE;
  CvtEndians(LBA, lsrb.CDBByte[2], 4);
  lsrb.CDBByte[1] := SecType;
  lsrb.CDBByte[7] := HiByte(TransferLength);
  lsrb.CDBByte[8] := LoByte(TransferLength);
  lsrb.CDBByte[9] := Other;
  lsrb.CDBByte[10] := SubChannelsSelection;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  ReserveTrack(ReservationSize: Cardinal): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $40;
  lsrb.CDBLen := $C;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $53;
  CvtEndians(ReservationSize, lsrb.cdbByte[5], 4);
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  ReadCapacity(var Capacity, SectorSize: Cardinal): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := 8;
  lsrb.BufPointer := @buf1;
  lsrb.CDBCmd := $25;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
  if Result then
  begin
    CvtEndians(buf1[4], SectorSize, 4);
    CvtEndians(buf1, Capacity, 4);
  end;
end;

function  Write10(LBA: Cardinal; TransferLength: Word; Buffer: PChar; buflen: Cardinal; Flags: Byte = $50): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := Flags;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := buflen;
  lsrb.BufPointer := buffer;
  lsrb.CDBCmd := $2A;
  CvtEndians(LBA, lsrb.CDBByte[2], 4);
  CvtEndians(TransferLength, lsrb.CDBByte[7], 2);
  ExecScsiCommand(lsrb, 120000);
  result := (lsrb.Status = SS_COMP) or (lsrb.Status = SS_PENDING);
end;

function  FlushCache;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $40;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $35;
  if Immed then
    lsrb.CDBByte[1] := $2;
  ExecScsiCommand(lsrb, TimeOut);
  result := lsrb.Status = SS_COMP;
end;

function  SetCDSpeed(ReadSpeed, WriteSpeed: Cardinal): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  if WriteSpeed = 0 then WriteSpeed := $FFFF else WriteSpeed := WriteSpeed;
  if ReadSpeed = 0 then ReadSpeed := $FFFF else ReadSpeed := ReadSpeed;
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $40;
  lsrb.CDBLen := $0C;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.CDBCmd := $BB;
  lsrb.CDBByte[2] := HiByte(ReadSpeed);
  lsrb.CDBByte[3] := LoByte(ReadSpeed);
  lsrb.CDBByte[4] := HiByte(WriteSpeed);
  lsrb.CDBByte[5] := LoByte(WriteSpeed);
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  ReadBufferCapacity(var BufferLength, BlankBufferLength: Cardinal): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  fillchar(buf1, $20, 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := $20;
  lsrb.BufPointer := @buf1;
  lsrb.CDBCmd := $5C;
  lsrb.CDBByte[8] := $20;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
  if result then
  begin
    CvtEndians(buf1[4], BufferLength, 4);
    CvtEndians(buf1[8], BlankBufferLength, 4);
    fDeviceBufferSize := BufferLength;
    fDeviceFreeBufferSize := BlankBufferLength;
    if BlankBufferLength = $FFFFFFFF then
      fDeviceFreeBufferSize := BufferLength;
  end
  else
  begin
    fDeviceFreeBufferSize := 0;

  end;
end;

Type
  TFormatCapacity = packed record
    Res1, Res2, Res3,
    CapacityListLength: Byte;
    NumberOfBlocks: Cardinal;
    BlockLength: Cardinal;
    NumberOfBlocks2: Cardinal;
    BlockLength2: Cardinal;
    NumberOfBlocks3: Cardinal;
    BlockLength3: Cardinal;
  end;

var
  bufFormatCapacity: TFormatCapacity;

function  GetFormatCapacity(var Capacity: Cardinal; var SectorSize: Cardinal): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  fillchar(bufFormatCapacity, sizeof(bufFormatCapacity), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $a;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := 248;
  lsrb.BufPointer := @bufFormatCapacity;
  lsrb.CDBCmd := $23;
  lsrb.CDBByte[8] := 248;
  ExecScsiCommand( lsrb , 10000);
  result := lsrb.Status = SS_COMP;
  if Result then
  begin
    bufFormatCapacity.NumberOfBlocks := L2MDW(bufFormatCapacity.NumberOfBlocks);
    bufFormatCapacity.NumberOfBlocks2 := L2MDW(bufFormatCapacity.NumberOfBlocks2);
    bufFormatCapacity.NumberOfBlocks3 := L2MDW(bufFormatCapacity.NumberOfBlocks3);
    SectorSize := 2048;
    if bufFormatCapacity.BlockLength = $080000 then
       Capacity := bufFormatCapacity.NumberOfBlocks
    else if (bufFormatCapacity.BlockLength2 = $080000) or (bufFormatCapacity.BlockLength2 = 152) then
       Capacity := bufFormatCapacity.NumberOfBlocks2
    else if bufFormatCapacity.BlockLength3 = $080000 then
       Capacity := bufFormatCapacity.NumberOfBlocks3
    else
       Capacity := bufFormatCapacity.NumberOfBlocks
  end;
end;

function  ReadTrackInformation;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  fillchar(fTrackInformation, sizeof(fTrackInformation), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := sizeof(fTrackInformation);
  lsrb.BufPointer := @fTrackInformation;
  lsrb.CDBCmd := $52;
  lsrb.CDBByte[1] := $01;
  lsrb.CDBByte[2] := HiByte(HiWord(TrackNumber));
  lsrb.CDBByte[3] := LoByte(HiWord(TrackNumber));
  lsrb.CDBByte[4] := HiByte(LoWord(TrackNumber));
  lsrb.CDBByte[5] := LoByte(LoWord(TrackNumber));
  lsrb.CDBByte[8] := sizeof(fTrackInformation);
  ExecScsiCommand(lsrb, 60000);
  result := lsrb.Status = SS_COMP;
  if result then
  begin
    fTrackInformation.Datalength := L2MW(fTrackInformation.Datalength);
    fTrackInformation.TrackSize := L2MDW(fTrackInformation.TrackSize);
    fTrackInformation.FreeBlocks := L2MDW(fTrackInformation.FreeBlocks);
    fTrackInformation.TrackStartAddress  := L2MDW(fTrackInformation.TrackStartAddress);
    fTrackInformation.NextWritableAddress := L2MDW(fTrackInformation.NextWritableAddress);
    fTrackInformation.FixedpacketSize := L2MDW(fTrackInformation.FixedpacketSize);
    fTrackInformation.LastRecordedAddress := L2MDW(fTrackInformation.LastRecordedAddress);
  end;
end;

function MSF2LBA(h, m, s, f: Byte): Cardinal;
begin
  result := (h * 60 * 60 * 75) + (m * 60 * 75)+(s * 75) + (f);
end;

var
  TOCData0000: TTOCData0000;
  TOCData0001: TTOCData0001;


function  GetDiscInformation: TDisc;
var
  ti, dt: Byte;
begin
  fillchar(fDisc, Sizeof(fDisc), 0);
  fDisc.DiscType := '';
  if TestUnitReady then
    fDisc.Valid := True
  else
  begin
    WaitForReady(3000);
    if TestUnitReady then
      fDisc.Valid := True
    else
    begin
      fDisc.Valid := False;
      exit;
    end;
  end;
  fillchar(TOCData0000, sizeof(TOCData0000), 0);
  if ReadTOC(0, @TOCData0000, Sizeof(TOCData0000)) then
  begin
    TOCData0000.DataLength := L2MW(TOCData0000.DataLength);
    if ReadTOC(0, @TOCData0000, Sizeof(TOCData0000), TOCData0000.LastTrackNumber) then
    begin
      TOCData0000.DataLength := L2MW(TOCData0000.DataLength);
      TOCData0000.TrackStartAddress := L2MDW(TOCData0000.TrackStartAddress);
    end;
    if ReadTOC(1, @TOCData0001, Sizeof(TOCData0001)) then
    begin
      fDisc.FirstCompleteSession := TOCData0001.FirstTrackNumber;
      fDisc.LastCompleteSession := TOCData0001.LastTrackNumber;
    end;
    if ReadTOC(4, @fTOC100, Sizeof(fTOC100), 0, True) then
      fDisc.TotalBlocks := MSF2LBA(0, fTOC100.EdMin, fTOC100.EdSec, fTOC100.EdFrame)
    else
      fDisc.TotalBlocks := 0;
    fDisc.DiscType := 'CD-ROM';
  end;
  fillchar(dtBuf, sizeof(dtBuf), 0);
  if GetConfigData(2, 0, @dtBuf, sizeof(dtBuf)) then
  begin
    dtBuf.CurrentProfile := L2MW(dtBuf.CurrentProfile);
    case dtBuf.CurrentProfile of
      $0001: fDisc.DiscType := 'Non-removable disk';
      $0002: fDisc.DiscType := 'Removable disk';
      $0003: begin fDisc.DiscType := 'MO Eraseable';  fDisc.Eraseable := true; end;
      $0004: fDisc.DiscType := 'MO Write Once';
      $0005: fDisc.DiscType := 'AS-MO';
      $0008: fDisc.DiscType := 'CD-ROM';
      $0009: fDisc.DiscType := 'CD-R';
      $000A: begin fDisc.DiscType := 'CD-RW'; fDisc.Eraseable := true; end;
      $0010: fDisc.DiscType := 'DVD-ROM';
      $0011: fDisc.DiscType := 'DVD-R';
      $0012: fDisc.DiscType := 'DVD-RAM';
      $0013: begin fDisc.DiscType := 'DVD-RW Restricted Overwrite'; fDisc.Eraseable := true; end;
      $0014: begin fDisc.DiscType := 'DVD-RW Sequential Recording'; fDisc.Eraseable := true; end;
      $001A: begin fDisc.DiscType := 'DVD+RW'; fDisc.Eraseable := true; end;
      $001B: fDisc.DiscType := 'DVD+RW';
      $0020: fDisc.DiscType := 'DDCD-ROM';
      $0021: fDisc.DiscType := 'DDCD-R';
      $0022: begin fDisc.DiscType := 'DDCD-RW'; fDisc.Eraseable := true; end;
       else  fDisc.DiscType := '???????';
    end;
    fDisc.DiscTypeCode := dtBuf.CurrentProfile;
    dt := DiscType;
    if dt >= mtDVD_ROM then
      ti := 1
    else
      ti := $FF;
    if not ReadTrackInformation(ti) then
    begin
      fDisc.UsedBlocks := fTrackInformation.NextWritableAddress;
      fDisc.TotalBlocks := fTrackInformation.NextWritableAddress + fTrackInformation.FreeBlocks;
      fDisc.BlockLength := 2048;
      ReadDiscInformation;
    end
    else
    begin
      if GetFormatCapacity(fDisc.UsedBlocks, fDisc.BlockLength) then
        if fDisc.TotalBlocks < fDisc.UsedBlocks then
          fDisc.TotalBlocks := fDisc.UsedBlocks;
    end;
  end;
  if ReadDiscInformation then with fDisc do
  begin

   if ((DiscInfo.DiscStatus and 00) = 00) then DiscStatus := dsEmptyDisc;
   if ((DiscInfo.DiscStatus and 01) = 01) then DiscStatus := dsIncompleteDisc; 
   if ((DiscInfo.DiscStatus and 02) = 02) then DiscStatus := dsCompleteDisc;
   if ((DiscInfo.DiscStatus and 03) = 03) then DiscStatus := dsOther;

   if ((DiscInfo.DiscStatus and 16) = 16) then Eraseable := true else Eraseable := False;
   DiscStatus := DiscStatus shl 4;
   DiscStatus := DiscStatus shr 6;
   if ((DiscInfo.DiscStatus and 00) = 00) then LastSession := lsEmptySession;
   if ((DiscInfo.DiscStatus and 01) = 01) then LastSession := lsIncompleteLastSession;
   if ((DiscInfo.DiscStatus and 02) = 02) then LastSession := lsDamagedLastSession;
   if ((DiscInfo.DiscStatus and 03) = 03) then LastSession := lsCompleteLastSession;

  end
  else
  begin
    ReadCapacity(fDisc.UsedBlocks, fDisc.BlockLength);
    if ReadTrackInformation($0) then
      fDisc.DiscType := 'CD-ROM';
  end;
  if fDisc.TotalBlocks < fDisc.UsedBlocks then
    fDisc.TotalBlocks := fDisc.UsedBlocks;
  result := fDisc;
end;

function  ReadDiscInformationRaw(Buffer: PChar; BufferLength: Integer): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  fillchar(DiscInfo, sizeof(DiscInfo), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := BufferLength;
  lsrb.BufPointer := Buffer;
  lsrb.CDBCmd := $51;
  lsrb.CDBByte[8] := BufferLength;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  ReadDiscInformation: Boolean;
begin
  fillchar(DiscInfo, sizeof(DiscInfo), 0);
  result := ReadDiscInformationRaw(@DiscInfo, sizeof(DiscInfo));
  if result then
  begin
    DiscInfo.Datalen := L2MW(DiscInfo.Datalen);
  end;
end;
{$R-}
function  LastSessionStatus: SmallInt;
var
  LastSession: Byte; 
begin

  if ReadDiscInformation then
  begin
    LastSession := lsEmptySession;
    DiscInfo.DiscStatus := DiscInfo.DiscStatus shl 4;
    DiscInfo.DiscStatus := DiscInfo.DiscStatus shr 6;
    if ((DiscInfo.DiscStatus and 01) = 01) then LastSession := lsIncompleteLastSession;
    if ((DiscInfo.DiscStatus and 02) = 02) then LastSession := lsDamagedLastSession;
    if ((DiscInfo.DiscStatus and 03) = 03) then LastSession := lsCompleteLastSession;
    result := LastSession;
  end
  else
    result := -1;
end;

function  DiscStatus: SmallInt;
var
  tDiscStatus: Byte;

begin
  if ReadDiscInformation then
  begin
    tDiscStatus := dsEmptyDisc;
    if ((DiscInfo.DiscStatus and 01) = 01) then
      tDiscStatus := dsIncompleteDisc; 
    if ((DiscInfo.DiscStatus and 02) = 02) then
      tDiscStatus := dsCompleteDisc;
    if ((DiscInfo.DiscStatus and 03) = 03) then
      tDiscStatus := dsOther;
    result := tDiscStatus;
  end
  else
    result := -1;
end;

function  SessionsOnDisc: Smallint;
var
  DiscStatus, LastSession: Byte;
begin
  if ReadDiscInformation then
  begin
   DiscStatus := dsEmptyDisc;
   if ((DiscInfo.DiscStatus and 01) = 01) then
     DiscStatus := dsIncompleteDisc;
   if ((DiscInfo.DiscStatus and 02) = 02) then
     DiscStatus := dsCompleteDisc;
   if ((DiscInfo.DiscStatus and 03) = 03) then
     DiscStatus := dsOther;
   DiscInfo.DiscStatus := DiscInfo.DiscStatus shl 4;
   DiscInfo.DiscStatus := DiscInfo.DiscStatus shr 6;
   LastSession := lsEmptySession;
   if ((DiscInfo.DiscStatus and 01) = 01) then LastSession := lsIncompleteLastSession;
   if ((DiscInfo.DiscStatus and 02) = 02) then LastSession := lsDamagedLastSession;
   if ((DiscInfo.DiscStatus and 03) = 03) then LastSession := lsCompleteLastSession;
   if (DiscStatus = dsEmptyDisc) then
   begin
     result := 0;
     exit;
   end;
   result := DiscInfo.Sessions;
   if (LastSession = lsEmptySession) and (result > 0) then
     dec(result);
  end
  else
    result := 0;
end;
{$R+}
type
  TVDesc = packed record
    pdType: Byte;
    Identifier: array[1..5] of char;
    Version: Byte;
    VolumeFlag: Byte;
    IdSystem: array[0..31] of char;
    IdVolume: array[0..31] of char;
    IdBC: array[0..7] of char;
    NoOfSectors: LONGWORD;
    NoOfSectorsBE: LONGWORD;
    Reserved: array[0..2048] of char;
  end;
var
  impvd: TVDesc;
function  GetLastRecordedAddress: Cardinal;
var
  i: Integer;
begin
  for i:=16 to 32 do
  begin
    fillchar(impvd, SizeOf(impvd), 0);
    if Read10(i, 1, @impvd, 2048) then
    with impvd do if (Identifier[1] = 'C') and (Identifier[2] = 'D') and (Identifier[3] = '0') and (Identifier[4] = '0') and (Identifier[5] = '1') then if (PdType = 2) or (PdType = 1) then
    begin
      result := impvd.NoOfSectors;
      exit;
    end;
  end;
  result := 0;
end;

function  UsedBlocksOnDisc: Cardinal;
var
  dt, ti: Byte;
  OrgFin: Boolean;
  SectorSize, UsedBlocks: Cardinal;
  
begin
  dt := DiscType;
  OrgFin := fFinalizeDisc;
  fFinalizeDisc := True;
  SetWriteParams(False, fUnderrunProtection, fFinalizeDisc, MediumIs);
  fFinalizeDisc := OrgFin;
  DiscInfo.LastTrackOfLastSession := 0;
  ReadDiscInformation;
  result := 0;

  if (dt < mtDVD_ROM) then
  begin
    Result := 0;
    ti := SessionsOnDisc+1;
    if ReadTrackInformation(ti) then
    begin
      result := fTrackInformation.NextWritableAddress;
      if fTrackInformation.NextWritableAddress = $FFFFFFFF then
        result := fTrackInformation.TrackSize;
      if not ((ti <> 1) and (result = 0)) then
        exit;
    end;
    if not ReadCapacity(UsedBlocks, SectorSize) then
    begin
      ti := SessionsOnDisc+1;
      if ReadTrackInformation(ti) then
        result := fTrackInformation.FreeBlocks;
    end
    else
    begin
      if UsedBlocks > $7F000000 then
      begin
        if GetFormatCapacity(UsedBlocks, SectorSize) then
          result := UsedBlocks
        else
          result := 0;
      end
      else
        result := UsedBlocks;
    end;
     exit;
  end;
  if ReadTrackInformation(DiscInfo.LastTrackOfLastSession) then
  begin
    if fTrackInformation.TrackSize = $FFFFFFFF then
    begin
      result := 0;
      exit;
    end;
    if (dt = mtDVD_RW_RO) then
    begin
      if GetFormatCapacity(UsedBlocks, SectorSize) then
      begin
         if fTrackInformation.TrackSize = fTrackInformation.FreeBlocks then
         begin
           result := 0;
         end
         else
           result := fTrackInformation.TrackSize;
         exit;
      end;
    end
    else if (dt = mtDVD_RW_SR) then
    begin
      if GetFormatCapacity(UsedBlocks, SectorSize) then
      begin
         if fTrackInformation.TrackSize = fTrackInformation.FreeBlocks then
         begin
           result := 0;
         end
         else
           result := fTrackInformation.TrackSize;
         exit;
      end;
    end
    else if (dt = mtDVD_RAM) or (dt = mtDVD_PLUSRW) then
    begin
      result := GetLastRecordedAddress;
      exit;
    end;
    result := fTrackInformation.NextWritableAddress;
  end;
 end;
function  FreeBlocksOnDisc: Cardinal;
var
  dt, ti: Byte;
  OrgFin: Boolean;
  TotalBlocks, UsedBlocks, SectorSize: Cardinal;
begin
  dt := DiscType;
  OrgFin := fFinalizeDisc;
  fFinalizeDisc := True;
  SetWriteParams(False, fUnderrunProtection, fFinalizeDisc, MediumIs);
  fFinalizeDisc := OrgFin;
  DiscInfo.LastTrackOfLastSession := 0;
  ReadDiscInformation;
  result := 0;
  if (dt < mtDVD_ROM) then
  begin
    OrgFin := fFinalizeDisc;
    fFinalizeDisc := True;
    SetWriteParams(False, fUnderrunProtection, fFinalizeDisc, MediumIs);
    fFinalizeDisc := OrgFin;
    if (DiscStatus = dsCompleteDisc) then
    begin
      result := 0;
      exit;
    end;
    ReadDiscInformation;
    ti := DiscInfo.LastTrackOfLastSession;
    if ReadTrackInformation(ti) then
    begin
      result := fTrackInformation.FreeBlocks;
      if fTrackInformation.FreeBlocks < fTrackInformation.TrackSize then
        result := fTrackInformation.TrackSize;
    end
    else
    begin
      if ReadTrackInformation(ti) then
      begin
        result := fTrackInformation.FreeBlocks;
        if fTrackInformation.FreeBlocks < fTrackInformation.TrackSize then
          result := fTrackInformation.TrackSize;
      end
      else
        result := 0;
    end;
    if fTrackInformation.FreeBlocks < fTrackInformation.TrackSize then
      result := fTrackInformation.TrackSize;
    exit;
  end;
  if ReadTrackInformation(DiscInfo.LastTrackOfLastSession) then
  begin
    if (dt = mtDVD_RW_RO) then
    begin
      if (fTrackInformation.NextWritableAddress <> 0) then
      begin
        result := fTrackInformation.FreeBlocks;
        exit;
      end;
      if GetFormatCapacity(TotalBlocks, SectorSize) then
      begin
         if fTrackInformation.TrackSize <> fTrackInformation.FreeBlocks then
           result := TotalBlocks - fTrackInformation.TrackSize
         else
           result := fTrackInformation.FreeBlocks;
         exit;
      end;
    end
    else if (dt = mtDVD_RAM) or (dt = mtDVD_PLUSRW) then
    begin
      UsedBlocks := GetLastRecordedAddress;
      result := fTrackInformation.TrackSize - UsedBlocks;
      exit;
    end;
    if fTrackInformation.TrackSize = $FFFFFFFF then
    begin
      result := 0;
      exit;
    end;
    if fTrackInformation.LastRecordedAddress = 0 then
      result := fTrackInformation.TrackSize
    else
      result := fTrackInformation.FreeBlocks;
  end;
  
end;

function  TotalBlocksOnDisc: Cardinal;
var
  dt, ti: Byte;
  OrgFin: Boolean;
  a, b : Cardinal;
  TotalBlocks, SectorSize: Cardinal;
label skip;  
begin
  dt := DiscType;
  OrgFin := fFinalizeDisc;
  fFinalizeDisc := True;
  SetWriteParams(False, fUnderrunProtection, fFinalizeDisc, MediumIs);
  fFinalizeDisc := OrgFin;
  DiscInfo.LastTrackOfLastSession := 0;
  ReadDiscInformation;
  result := 0;
  if (dt < mtDVD_ROM) then
  begin
    ti := SessionsOnDisc;
    if ReadTOC(4, @fTOC100, Sizeof(fTOC100), 0, True) then
    begin
      result := MSF2LBA(0, fTOC100.EdMin, fTOC100.EdSec, fTOC100.EdFrame);
      if result < 20000 then if ReadCapacity(a, b) then
       if a > result then result := a;
    end
    else if GetFormatCapacity(Result, Result) then
    begin
      if (Result = 4294770689) then
      begin
        result := 0;
        if ReadTrackInformation(ti+1) then
        result := fTrackInformation.FreeBlocks+fTrackInformation.TrackStartAddress
        else
        if ReadTrackInformation(ti) then
        result := fTrackInformation.FreeBlocks+fTrackInformation.TrackStartAddress;
      end;
      goto skip
    end
    else if ReadTrackInformation(ti) then 
    begin
      result := fTrackInformation.FreeBlocks+fTrackInformation.TrackStartAddress;
      exit;
    end
    else if GetFormatCapacity(Result, Result) then
      exit;
  skip:
    if Result = $FFFFFFFF then
      result := UsedBlocksOnDisc;
    exit;

  end;
  if ReadTrackInformation(DiscInfo.LastTrackOfLastSession) then
  begin
    if (dt = mtDVD_RAM) or (dt = mtDVD_PLUSRW) or (dt = mtDVD_RW_RO) then
    begin
      if GetFormatCapacity(TotalBlocks, SectorSize) then
      begin
         result := TotalBlocks;
         exit;
      end;
    end;
    result := fTrackInformation.FreeBlocks + fTrackInformation.NextWritableAddress;
  end;
end;

function  DeInitializeASPI: Boolean;
begin
  result := _DeInitializeASPI; 
end;

function  Erasable: Boolean;
begin
  Result := False;
  if ReadDiscInformation then
  begin
    if ((DiscInfo.DiscStatus and 16) = 16) then
      Result := True;
  end
  else
  begin
    MediumIs := DiscType;
    if (MediumIs = mtDVD_RAM) or (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) or (MediumIs = mtDVD_PLUSRW) then
      Result := True;
  end;
end;

function  DiscType: Byte;
var
  res: Boolean;
begin
  result := 0;
  res := GetConfigData(2, 0, @dtBuf, sizeof(dtbuf));
  if res then
  begin
    dtBuf.CurrentProfile := L2MW(dtBuf.CurrentProfile);
    case dtBuf.CurrentProfile of
      $0008: result := mtCD_ROM;
      $0009: result := mtCD_R;
      $000A: result := mtCD_RW;
      $0010: result := mtDVD_ROM;
      $0011: result := mtDVD_R;
      $0012: result := mtDVD_RAM;
      $0013: result := mtDVD_RW_RO;
      $0014: result := mtDVD_RW_SR;
      $001A: result := mtDVD_PLUSRW;
      $001B: result := mtDVD_PLUSR;
      $0020: result := mtDDCD_ROM;
      $0021: result := mtDDCD_R;
      $0022: result := mtDDCD_RW;
      else   result := mtUNKNOWN;
    end;
  end;
  if result = 0 then
  begin
    if ReadTOC(4, @fTOC100, Sizeof(fTOC100), 0, True) then
    begin
      if (fTOC100.Field2 and 128 = 128) then 
      begin
        if (fTOC100.Field2 and 64 = 64) then
          result := mtCD_RW
        else
          result := mtCD_R;
      end
      else
        result := mtCD_ROM;
    end;

    if ReadDiscInformation and (result = 0) then
    begin
      if ((DiscInfo.DiscStatus and 16) = 16) then
        result := mtCD_RW
      else
        result := mtCD_R;
    end;

    if (result = 0) and TestUnitReady then
      result := mtCD_ROM
  end;
end;

procedure  Lock;
begin
  CriticalSection.Enter;
end;

procedure  Unlock;
begin
  CriticalSection.Leave;
end;

function  ReadTOC(Format: Byte; buffer: PChar; BufferLength: Integer; SessionTrackNumber: Byte = 0; Time: Boolean = False; Control: Byte = 0): Boolean;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $0A;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := BufferLength;
  lsrb.BufPointer := Buffer;
  lsrb.CDBCmd := $43;
  if Time then
    lsrb.CDBByte[1] := 2;
  lsrb.CDBByte[2] := Format;
  lsrb.CDBByte[6] := SessionTrackNumber;
  lsrb.CDBByte[8] := BufferLength;
  lsrb.CDBByte[9] := Control;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

function  RequestSense;
var
  lsrb: TSRB_ExecSCSICmd;
begin
  fillchar(lsrb, sizeof(lsrb), 0);
  lsrb.Flags := $48;
  lsrb.CDBLen := $06;
  lsrb.SenseLen := SENSE_LEN;
  lsrb.BufLen := Len;
  lsrb.BufPointer := Buffer;
  lsrb.CDBCmd := $03;
  lsrb.CDBByte[4] := Len;
  ExecScsiCommand(lsrb, 10000);
  result := lsrb.Status = SS_COMP;
end;

var
 res: integer;
 str: String;
begin
  str := '';
  for res:=0 to 254 do
  CommandName[res] := IntToHex(res, 2);
  CommandName[$00] := 'TEST UNIT READY       ';
  CommandName[$01] := 'REWIND UNIT           ';
  CommandName[$03] := 'REQUEST SENSE         ';
  CommandName[$04] := 'FORMAT UNIT           ';
  CommandName[$12] := 'INQUIRY               ';
  CommandName[$1B] := 'START STOP UNIT       ';
  CommandName[$1e] := 'PREVENT/ALLOW REMOVAL ';
  CommandName[$23] := 'READ FORMAT CAPACITIES';
  CommandName[$25] := 'READ CAPACITY         ';
  CommandName[$28] := 'READ (10)             ';
  CommandName[$2a] := 'WRITE (10)            ';
  CommandName[$2b] := 'SEEK (10)             ';
  CommandName[$2e] := 'WRITE AND VERIFY (10) ';
  CommandName[$2f] := 'VERIFY (10)           ';
  CommandName[$35] := 'SYNCHRONIZE CACHE     ';
  CommandName[$42] := 'READ SUB-CHANNEL      ';
  CommandName[$43] := 'READ TOC/PMA/ATIP     ';
  CommandName[$45] := 'PLAY AUDIO (10)       ';
  CommandName[$46] := 'GET CONFIGURATION     ';
  CommandName[$47] := 'PLAY AUDIO MSF        ';
  CommandName[$4a] := 'GET EVENT/STATUS NOTIF';
  CommandName[$4b] := 'PAUSE/RESUME          ';
  CommandName[$4e] := 'STOP PLAY/SCAN        ';
  CommandName[$51] := 'READ DISC INFORMATION ';
  CommandName[$52] := 'READ TRACK INFORMATION';
  CommandName[$53] := 'RESERVE TRACK         ';
  CommandName[$54] := 'SEND OPC INFORMATION  ';
  CommandName[$55] := 'MODE SELECT (10)      ';
  CommandName[$58] := 'REPAIR TRACK          ';
  CommandName[$5a] := 'MODE SENSE (10)       ';
  CommandName[$5b] := 'CLOSE TRACK/SESSION   ';
  CommandName[$5c] := 'READ BUFFER CAPACITY  ';
  CommandName[$5d] := 'SEND CUE SHEET        ';
  CommandName[$a1] := 'BLANK                 ';
  CommandName[$a2] := 'SEND EVENT            ';
  CommandName[$a3] := 'SEND KEY              ';
  CommandName[$a4] := 'REPORT KEY            ';
  CommandName[$a5] := 'PLAY AUDIO (12)       ';
  CommandName[$a6] := 'LOAD/UNLOAD CD/DVD    ';
  CommandName[$a7] := 'SET READ AHEAD        ';
  CommandName[$a8] := 'READ (12)             ';
  CommandName[$aa] := 'WRITE (12)            ';
  CommandName[$ac] := 'GET PERFORMANCE       ';
  CommandName[$ad] := 'READ DVD STRUCTURE    ';
  CommandName[$b6] := 'SET STREAMING         ';
  CommandName[$b9] := 'READ CD MSF           ';
  CommandName[$ba] := 'SCAN                  ';
  CommandName[$bb] := 'SET CD SPEED          ';
  CommandName[$bd] := 'MECHANISM STATUS      ';
  CommandName[$be] := 'READ CD               ';
  CommandName[$bf] := 'SEND DVD STRUCTURE    ';

end.
