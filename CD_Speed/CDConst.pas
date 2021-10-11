unit CDConst;

interface

const
  mtUNKNOWN = 0;
  mtMESSAGE = 1;
  mtWARNING = 2;
  mtNONFATALERROR = 3;
  mtFATALERROR = 4;

resourcestring

  ERR_AS_00_00 = 'NO ADDITIONAL SENSE INFORMATION';
  ERR_AS_00_06 = 'I/O PROCESS TERMINATED';
  ERR_AS_00_11 = 'AUDIO PLAY OPERATION IN PROGRESS';
  ERR_AS_00_12 = 'AUDIO PLAY OPERATION PAUSED';
  ERR_AS_00_13 = 'AUDIO PLAY OPERATION SUCCESSFULLY COMPLETED';
  ERR_AS_00_14 = 'AUDIO PLAY OPERATION STOPPED DUE TO ERROR';
  ERR_AS_00_15 = 'NO CURRENT AUDIO STATUS TO RETURN';
  ERR_AS_00_16 = 'OPERATION IN PROGRESS';
  ERR_AS_00_17 = 'CLEANING REQUESTED';
  ERR_AS_02_00 = 'NO SEEK COMPLETE';
  ERR_AS_04_00 = 'LOGICAL UNIT NOT READY, CAUSE NOT REPORTABLE';
  ERR_AS_04_01 = 'LOGICAL UNIT IS IN PROCESS OF BECOMING READY';
  ERR_AS_04_02 = 'LOGICAL UNIT NOT READY, INITIALIZING CMD. REQUIRED';
  ERR_AS_04_03 = 'LOGICAL UNIT NOT READY, MANUAL INTERVENTION REQUIRED';
  ERR_AS_04_04 = 'LOGICAL UNIT NOT READY, FORMAT IN PROGRESS';
  ERR_AS_04_07 = 'LOGICAL UNIT NOT READY, OPERATION IN PROGRESS';
  ERR_AS_04_08 = 'LOGICAL UNIT NOT READY, LONG WRITE IN PROGRESS';
  ERR_AS_04_09 = 'LOGICAL UNIT NOT READY, SELF-TEST IN PROGRESS';
  ERR_AS_05_00 = 'LOGICAL UNIT DOES NOT RESPOND TO SELECTION';
  ERR_AS_06_00 = 'NO REFERENCE POSITION FOUND';
  ERR_AS_07_00 = 'MULTIPLE PERIPHERAL DEVICES SELECTED';
  ERR_AS_08_00 = 'LOGICAL UNIT COMMUNICATION FAILURE';
  ERR_AS_08_01 = 'LOGICAL UNIT COMMUNICATION TIME-OUT';
  ERR_AS_08_02 = 'LOGICAL UNIT COMMUNICATION PARITY ERROR';
  ERR_AS_08_03 = 'LOGICAL UNIT COMMUNICATION CRC ERROR (ULTRA-DMA/32)';
  ERR_AS_08_04 = 'UNREACHABLE COPY TARGET';
  ERR_AS_09_00 = 'TRACK FOLLOWING ERROR';
  ERR_AS_09_01 = 'TRACKING SERVO FAILURE';
  ERR_AS_09_02 = 'FOCUS SERVO FAILURE';
  ERR_AS_09_03 = 'SPINDLE SERVO FAILURE';
  ERR_AS_09_04 = 'HEAD SELECT FAULT';
  ERR_AS_0A_00 = 'ERROR LOG OVERFLOW';
  ERR_AS_0B_00 = 'WARNING';
  ERR_AS_0B_01 = 'WARNING - SPECIFIED TEMPERATURE EXCEEDED';
  ERR_AS_0B_02 = 'WARNING - ENCLOSURE DEGRADED';
  ERR_AS_0C_00 = 'WRITE ERROR';
  ERR_AS_0C_07 = 'WRITE ERROR - RECOVERY NEEDED';
  ERR_AS_0C_08 = 'WRITE ERROR - RECOVERY FAILED';
  ERR_AS_0C_09 = 'WRITE ERROR - LOSS OF STREAMING';
  ERR_AS_0C_0A = 'WRITE ERROR - PADDING BLOCKS ADDED';
  ERR_AS_0D_00 = 'ERROR DETECTED BY THIRD PARTY TEMPORARY INITIATOR';
  ERR_AS_0D_01 = 'THIRD PARTY DEVICE FAILURE';
  ERR_AS_0D_02 = 'COPY TARGET DEVICE NOT REACHABLE';
  ERR_AS_0D_03 = 'INCORRECT COPY TARGET DEVICE TYPE';
  ERR_AS_0D_04 = 'COPY TARGET DEVICE DATA UNDERRUN';
  ERR_AS_0D_05 = 'COPY TARGET DEVICE DATA OVERRUN';
  ERR_AS_11_00 = 'UNRECOVERED READ ERROR';
  ERR_AS_11_01 = 'READ RETRIES EXHAUSTED';
  ERR_AS_11_02 = 'ERROR TOO LONG TO CORRECT';
  ERR_AS_11_05 = 'L-EC UNCORRECTABLE ERROR';
  ERR_AS_11_06 = 'CIRC UNRECOVERED ERROR';
  ERR_AS_11_0D = 'DE-COMPRESSION CRC ERROR';
  ERR_AS_11_0E = 'CANNOT DECOMPRESS USING DECLARED ALGORITHM';
  ERR_AS_11_0F = 'ERROR READING UPC/EAN NUMBER';
  ERR_AS_11_10 = 'ERROR READING ISRC NUMBER';
  ERR_AS_11_11 = 'READ ERROR - LOSS OF STREAMING';
  ERR_AS_14_00 = 'RECORDED ENTITY NOT FOUND';
  ERR_AS_14_01 = 'RECORD NOT FOUND';
  ERR_AS_15_00 = 'RANDOM POSITIONING ERROR';
  ERR_AS_15_01 = 'MECHANICAL POSITIONING ERROR';
  ERR_AS_15_02 = 'POSITIONING ERROR DETECTED BY READ OF MEDIUM';
  ERR_AS_17_00 = 'RECOVERED DATA WITH NO ERROR CORRECTION APPLIED';
  ERR_AS_17_01 = 'RECOVERED DATA WITH RETRIES';
  ERR_AS_17_02 = 'RECOVERED DATA WITH POSITIVE HEAD OFFSET';
  ERR_AS_17_03 = 'RECOVERED DATA WITH NEGATIVE HEAD OFFSET';
  ERR_AS_17_04 = 'RECOVERED DATA WITH RETRIES AND/OR CIRC APPLIED';
  ERR_AS_17_05 = 'RECOVERED DATA USING PREVIOUS SECTOR ID';
  ERR_AS_17_07 = 'RECOVERED DATA WITHOUT ECC - RECOMMEND REASSIGNMENT';
  ERR_AS_17_08 = 'RECOVERED DATA WITHOUT ECC - RECOMMEND REWRITE';
  ERR_AS_17_09 = 'RECOVERED DATA WITHOUT ECC - DATA REWRITTEN';
  ERR_AS_18_00 = 'RECOVERED DATA WITH ERROR CORRECTION APPLIED';
  ERR_AS_18_01 = 'RECOVERED DATA WITH ERROR CORR. & RETRIES APPLIED';
  ERR_AS_18_02 = 'RECOVERED DATA - DATA AUTO-REALLOCATED';
  ERR_AS_18_03 = 'RECOVERED DATA WITH CIRC';
  ERR_AS_18_04 = 'RECOVERED DATA WITH L-EC';
  ERR_AS_18_05 = 'RECOVERED DATA - RECOMMEND REASSIGNMENT';
  ERR_AS_18_06 = 'RECOVERED DATA - RECOMMEND REWRITE';
  ERR_AS_18_08 = 'RECOVERED DATA WITH LINKING';
  ERR_AS_1A_00 = 'PARAMETER LIST LENGTH ERROR';
  ERR_AS_1B_00 = 'SYNCHRONOUS DATA TRANSFER ERROR';
  ERR_AS_1D_00 = 'MISCOMPARE DURING VERIFY OPERATION';
  ERR_AS_20_00 = 'INVALID COMMAND OPERATION CODE';
  ERR_AS_21_00 = 'LOGICAL BLOCK ADDRESS OUT OF RANGE';
  ERR_AS_21_01 = 'INVALID ELEMENT ADDRESS';
  ERR_AS_21_02 = 'INVALID ADDRESS FOR WRITE';
  ERR_AS_24_00 = 'INVALID FIELD IN CDB';
  ERR_AS_24_01 = 'CDB DECRYPTION ERROR';
  ERR_AS_25_00 = 'LOGICAL UNIT NOT SUPPORTED';
  ERR_AS_26_00 = 'INVALID FIELD IN PARAMETER LIST';
  ERR_AS_26_01 = 'PARAMETER NOT SUPPORTED';
  ERR_AS_26_02 = 'PARAMETER VALUE INVALID';
  ERR_AS_26_03 = 'THRESHOLD PARAMETERS NOT SUPPORTED';
  ERR_AS_26_04 = 'INVALID RELEASE OF PERSISTENT RESERVATION';
  ERR_AS_26_05 = 'DATA DECRYPTION ERROR';
  ERR_AS_26_06 = 'TOO MANY TARGET DESCRIPTORS';
  ERR_AS_26_07 = 'UNSUPPORTED TARGET DESCRIPTOR TYPE CODE';
  ERR_AS_26_08 = 'TOO MANY SEGMENT DESCRIPTORS';
  ERR_AS_26_09 = 'UNSUPPORTED SEGMENT DESCRIPTOR TYPE CODE';
  ERR_AS_26_0A = 'UNEXPECTED INEXACT SEGMENT';
  ERR_AS_26_0B = 'INLINE DATA LENGTH EXCEEDED';
  ERR_AS_26_0C = 'INVALID OPERATION FOR COPY SOURCE OR DESTINATION';
  ERR_AS_26_0D = 'COPY SEGMENT GRANULARITY VIOLATION';
  ERR_AS_27_00 = 'WRITE PROTECTED';
  ERR_AS_27_01 = 'HARDWARE WRITE PROTECTED';
  ERR_AS_27_02 = 'LOGICAL UNIT SOFTWARE WRITE PROTECTED';
  ERR_AS_27_03 = 'ASSOCIATED WRITE PROTECT';
  ERR_AS_27_04 = 'PERSISTENT WRITE PROTECT';
  ERR_AS_27_05 = 'PERMANENT WRITE PROTECT';
  ERR_AS_27_06 = 'CONDITIONAL WRITE PROTECT';
  ERR_AS_28_00 = 'NOT READY TO READY CHANGE, MEDIUM MAY HAVE CHANGED';
  ERR_AS_28_01 = 'IMPORT OR EXPORT ELEMENT ACCESSED';
  ERR_AS_29_00 = 'POWER ON, RESET, OR BUS DEVICE RESET OCCURRED';
  ERR_AS_29_01 = 'POWER ON OCCURRED';
  ERR_AS_29_02 = 'SCSI BUS RESET OCCURRED';
  ERR_AS_29_03 = 'BUS DEVICE RESET FUNCTION OCCURRED';
  ERR_AS_29_04 = 'DEVICE INTERNAL RESET';
  ERR_AS_29_05 = 'TRANSCEIVER MODE CHANGED TO SINGLE-ENDED';
  ERR_AS_29_06 = 'TRANSCEIVER MODE CHANGED TO LVD';
  ERR_AS_2A_00 = 'PARAMETERS CHANGED';
  ERR_AS_2A_01 = 'MODE PARAMETERS CHANGED';
  ERR_AS_2A_02 = 'LOG PARAMETERS CHANGED';
  ERR_AS_2A_03 = 'RESERVATIONS PREEMPTED';
  ERR_AS_2A_04 = 'RESERVATIONS RELEASED';
  ERR_AS_2A_05 = 'REGISTRATIONS PREEMPTED';
  ERR_AS_2B_00 = 'COPY CANNOT EXECUTE SINCE HOST CANNOT DISCONNECT';
  ERR_AS_2C_00 = 'COMMAND SEQUENCE ERROR';
  ERR_AS_2C_03 = 'CURRENT PROGRAM AREA IS NOT EMPTY';
  ERR_AS_2C_04 = 'CURRENT PROGRAM AREA IS EMPTY';
  ERR_AS_2C_06 = 'PERSISTENT PREVENT CONFLICT';
  ERR_AS_2E_00 = 'INSUFFICIENT TIME FOR OPERATION';
  ERR_AS_2F_00 = 'COMMANDS CLEARED BY ANOTHER INITIATOR';
  ERR_AS_30_00 = 'INCOMPATIBLE MEDIUM INSTALLED';
  ERR_AS_30_01 = 'CANNOT READ MEDIUM - UNKNOWN FORMAT';
  ERR_AS_30_02 = 'CANNOT READ MEDIUM - INCOMPATIBLE FORMAT';
  ERR_AS_30_03 = 'CLEANING CARTRIDGE INSTALLED';
  ERR_AS_30_04 = 'CANNOT WRITE MEDIUM - UNKNOWN FORMAT';
  ERR_AS_30_05 = 'CANNOT WRITE MEDIUM - INCOMPATIBLE FORMAT';
  ERR_AS_30_06 = 'CANNOT FORMAT MEDIUM - INCOMPATIBLE MEDIUM';
  ERR_AS_30_07 = 'CLEANING FAILURE';
  ERR_AS_30_08 = 'CANNOT WRITE - APPLICATION CODE MISMATCH';
  ERR_AS_30_09 = 'CURRENT SESSION NOT FIXATED FOR APPEND';
  ERR_AS_30_10 = 'MEDIUM NOT FORMATTED';
  ERR_AS_31_00 = 'MEDIUM FORMAT CORRUPTED';
  ERR_AS_31_01 = 'FORMAT COMMAND FAILED';
  ERR_AS_31_02 = 'ZONED FORMATTING FAILED DUE TO SPARE LINKING';
  ERR_AS_34_00 = 'ENCLOSURE FAILURE';
  ERR_AS_35_00 = 'ENCLOSURE SERVICES FAILURE';
  ERR_AS_35_01 = 'UNSUPPORTED ENCLOSURE FUNCTION';
  ERR_AS_35_02 = 'ENCLOSURE SERVICES UNAVAILABLE';
  ERR_AS_35_03 = 'ENCLOSURE SERVICES TRANSFER FAILURE';
  ERR_AS_35_04 = 'ENCLOSURE SERVICES TRANSFER REFUSED';
  ERR_AS_37_00 = 'ROUNDED PARAMETER';
  ERR_AS_39_00 = 'SAVING PARAMETERS NOT SUPPORTED';
  ERR_AS_3A_00 = 'MEDIUM NOT PRESENT';
  ERR_AS_3A_01 = 'MEDIUM NOT PRESENT - TRAY CLOSED';
  ERR_AS_3A_02 = 'MEDIUM NOT PRESENT - TRAY OPEN';
  ERR_AS_3A_03 = 'MEDIUM NOT PRESENT - LOADABLE';
  ERR_AS_3A_04 = 'MEDIUM NOT PRESENT - MEDIUM AUXILIARY MEMORY ACCESSIBLE';
  ERR_AS_3B_0D = 'MEDIUM DESTINATION ELEMENT FULL';
  ERR_AS_3B_0E = 'MEDIUM SOURCE ELEMENT EMPTY';
  ERR_AS_3B_0F = 'END OF MEDIUM REACHED';
  ERR_AS_3B_11 = 'MEDIUM MAGAZINE NOT ACCESSIBLE';
  ERR_AS_3B_12 = 'MEDIUM MAGAZINE REMOVED';
  ERR_AS_3B_13 = 'MEDIUM MAGAZINE INSERTED';
  ERR_AS_3B_14 = 'MEDIUM MAGAZINE LOCKED';
  ERR_AS_3B_15 = 'MEDIUM MAGAZINE UNLOCKED';
  ERR_AS_3B_16 = 'MECHANICAL POSITIONING OR CHANGER ERROR';
  ERR_AS_3D_00 = 'INVALID BITS IN IDENTIFY MESSAGE';
  ERR_AS_3E_00 = 'LOGICAL UNIT HAS NOT SELF-CONFIGURED YET';
  ERR_AS_3E_01 = 'LOGICAL UNIT FAILURE';
  ERR_AS_3E_02 = 'TIMEOUT ON LOGICAL UNIT';
  ERR_AS_3E_03 = 'LOGICAL UNIT FAILED SELF-TEST';
  ERR_AS_3E_04 = 'LOGICAL UNIT UNABLE TO UPDATE SELF-TEST LOG';
  ERR_AS_3F_00 = 'TARGET OPERATING CONDITIONS HAVE CHANGED';
  ERR_AS_3F_01 = 'MICROCODE HAS BEEN CHANGED';
  ERR_AS_3F_02 = 'CHANGED OPERATING DEFINITION';
  ERR_AS_3F_03 = 'INQUIRY DATA HAS CHANGED';
  ERR_AS_3F_04 = 'COMPONENT DEVICE ATTACHED';
  ERR_AS_3F_05 = 'DEVICE IDENTIFIER CHANGED';
  ERR_AS_3F_06 = 'REDUNDANCY GROUP CREATED OR MODIFIED';
  ERR_AS_3F_07 = 'REDUNDANCY GROUP DELETED';
  ERR_AS_3F_08 = 'SPARE CREATED OR MODIFIED';
  ERR_AS_3F_09 = 'SPARE DELETED';
  ERR_AS_3F_0A = 'VOLUME SET CREATED OR MODIFIED';
  ERR_AS_3F_0B = 'VOLUME SET DELETED';
  ERR_AS_3F_0C = 'VOLUME SET DEASSIGNED';
  ERR_AS_3F_0D = 'VOLUME SET REASSIGNED';
  ERR_AS_3F_0E = 'REPORTED LUNS DATA HAS CHANGED';
  ERR_AS_3F_0F = 'ECHO BUFFER OVERWRITTEN';
  ERR_AS_3F_10 = 'MEDIUM LOADABLE';
  ERR_AS_3F_11 = 'MEDIUM AUXILIARY MEMORY ACCESSIBLE';
  ERR_AS_43_00 = 'MESSAGE ERROR';
  ERR_AS_44_00 = 'INTERNAL TARGET FAILURE';
  ERR_AS_45_00 = 'SELECT OR RESELECT FAILURE';
  ERR_AS_46_00 = 'UNSUCCESSFUL SOFT RESET';
  ERR_AS_47_00 = 'SCSI PARITY ERROR';
  ERR_AS_47_01 = 'DATA PHASE CRC ERROR DETECTED';
  ERR_AS_47_02 = 'SCSI PARITY ERROR DETECTED DURING ST DATA PHASE';
  ERR_AS_47_03 = 'INFORMATION UNIT CRC ERROR DETECTED';
  ERR_AS_47_04 = 'ASYNCHRONOUS INFORMATION PROTECTION ERROR DETECTED';
  ERR_AS_48_00 = 'INITIATOR DETECTED ERROR MESSAGE RECEIVED';
  ERR_AS_49_00 = 'INVALID MESSAGE ERROR';
  ERR_AS_4A_00 = 'COMMAND PHASE ERROR';
  ERR_AS_4B_00 = 'DATA PHASE ERROR';
  ERR_AS_4C_00 = 'LOGICAL UNIT FAILED SELF-CONFIGURATION';
  ERR_AS_4E_00 = 'OVERLAPPED COMMANDS ATTEMPTED';
  ERR_AS_51_00 = 'ERASE FAILURE';
  ERR_AS_51_01 = 'ERASE FAILURE - INCOMPLETE ERASE OPERATION DETECTED';
  ERR_AS_53_00 = 'MEDIA LOAD OR EJECT FAILED';
  ERR_AS_53_02 = 'MEDIUM REMOVAL PREVENTED';
  ERR_AS_55_02 = 'INSUFFICIENT RESERVATION RESOURCES';
  ERR_AS_55_03 = 'INSUFFICIENT RESOURCES';
  ERR_AS_55_04 = 'INSUFFICIENT REGISTRATION RESOURCES';
  ERR_AS_57_00 = 'UNABLE TO RECOVER TABLE-OF-CONTENTS';
  ERR_AS_5A_00 = 'OPERATOR REQUEST OR STATE CHANGE INPUT';
  ERR_AS_5A_01 = 'OPERATOR MEDIUM REMOVAL REQUEST';
  ERR_AS_5A_02 = 'OPERATOR SELECTED WRITE PROTECT';
  ERR_AS_5A_03 = 'OPERATOR SELECTED WRITE PERMIT';
  ERR_AS_5B_00 = 'LOG EXCEPTION';
  ERR_AS_5B_01 = 'THRESHOLD CONDITION MET';
  ERR_AS_5B_02 = 'LOG COUNTER AT MAXIMUM';
  ERR_AS_5B_03 = 'LOG LIST CODES EXHAUSTED';
  ERR_AS_5D_00 = 'FAILURE PREDICTION THRESHOLD EXCEEDED';
  ERR_AS_5D_01 = 'MEDIA FAILURE PREDICTION THRESHOLD EXCEEDED';
  ERR_AS_5D_02 = 'LOGICAL UNIT FAILURE PREDICTION THRESHOLD EXCEEDED';
  ERR_AS_5D_03 = 'SPARE AREA EXHAUSTION PREDICTION THRESHOLD EXCEEDED';
  ERR_AS_5D_FF = 'FAILURE PREDICTION THRESHOLD EXCEEDED (FALSE)';
  ERR_AS_5E_00 = 'LOW POWER CONDITION ON';
  ERR_AS_5E_01 = 'IDLE CONDITION ACTIVATED BY TIMER';
  ERR_AS_5E_02 = 'STANDBY CONDITION ACTIVATED BY TIMER';
  ERR_AS_5E_03 = 'IDLE CONDITION ACTIVATED BY COMMAND';
  ERR_AS_5E_04 = 'STANDBY CONDITION ACTIVATED BY COMMAND';
  ERR_AS_63_00 = 'END OF USER AREA ENCOUNTERED ON THIS TRACK';
  ERR_AS_63_01 = 'PACKET DOES NOT FIT IN AVAILABLE SPACE';
  ERR_AS_64_00 = 'ILLEGAL MODE FOR THIS TRACK';
  ERR_AS_64_01 = 'INVALID PACKET SIZE';
  ERR_AS_65_00 = 'VOLTAGE FAULT';
  ERR_AS_6F_00 = 'COPY PROTECTION KEY EXCHANGE FAILURE - AUTHENTICATION FAILURE';
  ERR_AS_6F_01 = 'COPY PROTECTION KEY EXCHANGE FAILURE - KEY NOT PRESENT';
  ERR_AS_6F_02 = 'COPY PROTECTION KEY EXCHANGE FAILURE - KEY NOT ESTABLISHED';
  ERR_AS_6F_03 = 'READ OF SCRAMBLED SECTOR WITHOUT AUTHENTICATION';
  ERR_AS_6F_04 = 'MEDIA REGION CODE IS MISMATCHED TO LOGICAL UNIT REGION';
  ERR_AS_6F_05 = 'DRIVE REGION MUST BE PERMANENT/REGION RESET COUNT ERROR';
  ERR_AS_72_00 = 'SESSION FIXATION ERROR';
  ERR_AS_72_01 = 'SESSION FIXATION ERROR WRITING LEAD-IN';
  ERR_AS_72_02 = 'SESSION FIXATION ERROR WRITING LEAD-OUT';
  ERR_AS_72_03 = 'SESSION FIXATION ERROR - INCOMPLETE TRACK IN SESSION';
  ERR_AS_72_04 = 'EMPTY OR PARTIALLY WRITTEN RESERVED TRACK';
  ERR_AS_72_05 = 'NO MORE TRACK RESERVATIONS ALLOWED';
  ERR_AS_73_00 = 'CD CONTROL ERROR';
  ERR_AS_73_01 = 'POWER CALIBRATION AREA ALMOST FULL';
  ERR_AS_73_02 = 'POWER CALIBRATION AREA IS FULL';
  ERR_AS_73_03 = 'POWER CALIBRATION AREA ERROR';
  ERR_AS_73_04 = 'PROGRAM MEMORY AREA UPDATE FAILURE';
  ERR_AS_73_05 = 'PROGRAM MEMORY AREA IS FULL';
  ERR_AS_73_06 = 'RMA/PMA IS ALMOST FULL';

  ERR_INVALIDREQUEST = 'INVALID REQUEST';
  ERR_INVALIDHA = 'INVALID HOST ADAPTER';
  ERR_NODEVICE = 'NO DEVICE';
  ERR_INVALIDSRB = 'INVALID SRB';
  ERR_BUFFERALIGNMENT = 'BUFFER ALIGNMENT';
  ERR_ASPIBUSY = 'ASPI IS BUSY';
  ERR_BUFFERTOOBIG = 'BUFFER TOO BIG';
  ERR_TIMEOUT = 'COMMAND TIMEOUT';
  ERR_SRBTIMEOUT = 'SRB TIMEOUT';
  ERR_MESSAGEREJECT = 'MESSAGE REJECT';
  ERR_BUSRESET = 'BUS RESET';
  ERR_PARITYERR = 'PARITY ERROR';
  ERR_REQUESTSENSEFAILED = 'REQUEST SENSE FAILED';
  ERR_SELECTIONTIMEOUT = 'SELECTION TIMEOUT';
  ERR_DATAOVERRUN = 'DATA OVERRUN';
  ERR_UNEXPECTEDBUSFREE= 'UNEXPECTED BUS FREE';
  ERR_CHECKCONDITION = 'CHECK CONDITION';
  ERR_TARGETBUSY = 'TARGET BUSY';
  ERR_TARGETCONFLICT = 'TARGET RESERVATION CONFLICT';
  ERR_QUEFULL = 'TARGET QUEUE FULL';
  ERR_RECOVEREDERROR = 'RECOVERED ERROR';
  ERR_NOTREADY = 'NOT READY';
  ERR_MEDIUMERROR = 'MEDIUM ERROR';
  ERR_HARDWAREERROR = 'HARDWARE ERROR';
  ERR_ILLEGALREQUEST = 'ILLEGAL REQUEST';
  ERR_UNITATTENTION = 'UNIT ATTENTION';
  ERR_DATAPROTECT = 'DATA PROTECT';
  ERR_ERASECHECK = 'ERASE CHECK';
  ERR_COPYABORTED = 'COPYABORTED';
  ERR_ABORTEDCOMMAND = 'ABORTED COMMAND';
  ERR_VOLUMEOVERFLOW = 'VOLUME OVERFLOW';
  ERR_MISCOMPARE = 'MISCOMPARE';
  ERR_RESERVED = 'RESERVED';
  ERR_FILEMARK = 'FILEMARK';
  ERR_ENDOFMEDIA = 'END OF MEDIA';
  ERR_ILLEGALLENGTH = 'ILLEGAL LENGTH';
  ERR_INCORRECTLENGTH = 'INCORRECT LENGTH';

  ERR_NONE = '';
  ERR_UNKNOWN = 'UNKNOWN';
  ERR_NOERROR = 'NO ERROR';
  ERR_ABORTED = 'OPERATION ABORTED BY USER';
  ERR_FILEINUSE = 'CAN''T OPEN FILE "%s" OR FILE IS IN USES BY OTHER SOFTWARE';
  ERR_CREATEFILE = 'CAN''T CREATE FILE "%s"';
  ERR_DEVICEBUSY = 'WRITER BUSY/BUFFER FULL';
  ERR_WRITEERROR = 'WRITER ERROR';
  ERR_INVALIDDEVICE = 'INVALID DEVICE ID / NOT A CD OR DVD DRIVE';
  ERR_NOTSUPPORTED = 'NOT SUPPORTED';
  ERR_NEXTADRESS = 'ERROR GETTING NEXT WRITABLE ADDRESS';
  ERR_TOOMUCHDATA = 'TOO MUCH DATA FOR THIS MODE';

  ERR_MAXDIRS = 'MAXIMUM DIRECTORIES CAN BE %d';
  ERR_MAXFILES = 'MAXIMUM FILES CAN BE %d';
  ERR_INVALIDDESTDIR = 'INVALID DESTINATION PATH';
  ERR_INVALIDFILENAME = 'ERROR IMPORTING SESSION, FILE NAME LENGTH EXCEEDS 120 CHARS';

  ERR_IMPORTSESSION = 'ERROR IMPORTING SESSION';
  ERR_ISOIMAGENOTFOUND = 'ERROR IMPORTING SESSION %d (ISO IMAGE NOT FOUND)';
  ERR_1 = 'INTERNAL ERROR 0001';
  ERR_2 = 'INTERNAL ERROR 0002';
  ERR_3 = 'INTERNAL ERROR 0003';
  ERR_4 = 'INTERNAL ERROR 0004';
  ERR_5 = 'INTERNAL ERROR 0005';
  ERR_TRIALLIMIT = 'TRIAL VERSION - CAN''T BURN MORE THAN 128MB OF DATA';
  ERR_NONEMPTYDISC = 'NEED EMPTY DISC FOR ISO BURNING';

  ERR_SREADERROR = 'Stream read error';
  ERR_SWRITEERROR = 'Stream write error';
  ERR_SInvalidImage = 'Invalid stream format';

  CUESEND_ERR   = 'SEND CUE SHEET FAILED';
  CUEFILE_ERR_1 = 'UNEXPECTED END OF LINE';
  CUEFILE_ERR_2 = 'INVALID FIELD "%s" IN PARAMETER LIST';
  CUEFILE_ERR_3 = 'FILE NOT FOUND';
  CUEFILE_ERR_4 = 'INVALID COMMAND "%s"';

  MSG_WAIT15MIN  = 'WRITING LEAD-OUT (THIS CAN TAKE APPROXIMATELY 15-20 MINUTES)';
  MSG_WRITESTART = 'STARTING WRITE PROCESS ON %s AT %s';
  MSG_ERASESTART = 'STARTING ERASE PROCESS ON %s AT %s';
  MSG_EXTRACTING_FILE = 'EXTRACTING FILE %s TO %s';
  MSG_TESTWRITE  = 'TEST/DUMMY WRITE';
  MSG_IMPORTINGSESSION = 'IMPORTING SESSION # %d';
implementation

end.

