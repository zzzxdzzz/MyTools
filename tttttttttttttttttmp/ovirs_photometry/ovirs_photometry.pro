; docformat = 'rst'

;+
; Pipeline for SAWG OVIRS PHOTOMETRY
; `ovirs_photometry` processes the OVIRS I/F FITS files to produce RADF, PC I/F, and BOND files for SAWG
;
; :Author:
;    ${user}
;
; :History:
;    ${user}, ${date}: initial template
;
;-


;+
; :Description:
;    Process the command line arguments
;    Catch errors and set exit status
;    Run .PhotModS, PhotCorrS, and BoloAlbedoS in order according to the command line arguments
;
; ${parameters}
; cwd - the working directory
; iof - the OVIRS I/F FITS filename (used by PhotModS, PhotCorrS, and BoloAlbedoS)
; radf - the RADF FITS filename (created by PhotModS, used by PhotCorrS and BoloAlbedoS)
; pciof - the PhotModS filename to to use for output
; bond - the BoloAlbedoS filename to to use for output
; These file names will be passed as command line arguments via a system call to the pipeline
;
; :Example:
; idl -rt=ovirs_photometry.sav -args iof.fits radf.fits pciof.fits bond.fits
;
; :History:
;     ${user},  ${date}   Add your comment here
;-
pro ovirs_photometry

  ; Report exceptions where/when they occur
  !EXCEPT=2
  module = 'init'

  ; Return control to the calling procedure when an error occurs
  ; The CATCH statement will process the error, and then return to the higher routine
  CATCH, Error_status

  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
    PRINT, 'Error in module: ', module
    PRINT, 'Error index: ', Error_status
    PRINT, 'Error message: ', !ERROR_STATE.MSG
    Help, /Last_Message, Output=traceback
    Print, Format='(A)', traceback
    exit, status=Error_status
  ENDIF

  ; Retrieve the command line arguments to the pipeline
  args = command_line_args(count=nargs)
  kw_args = Where(StRegEx(args, '=', /Boolean), nkw, Complement=cl_args)
  If (nkw GT 0) Then Begin
    kw_args = args(kw_args)
    args = args(cl_args)
    nargs = nargs - nkw
    
    kw = {}
    ForEach arg, kw_args Do Begin
      SET = StrSplit(arg, '=', /Extract)
      ;help, set
      key = StrTrim(set[0], 2)
      value = StrTrim(set[1], 2)
      isInt = StRegEx(value, '^[-+]?[0-9]+$', /Boolean)
      isFloat = StRegEx(value, '^[-+]?([0-9]+\.?[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?$', /Boolean)
      If isInt Then value = Long(value) Else If isFloat Then value = Double(value)
      kw = Create_Struct(kw, key, value)
    EndForEach
    help, kw
    help, /struct, kw
  EndIf  
  
  If File_Test(args[0], /Directory) Then Begin
  cd, args[0], curr=curr
  print, 'previous dir: ', curr, '  now: ', args[0]
  args = args[1:*]
  EndIf
  
  ; Run PhotModS if the radf file does not exist
  IF NOT File_Test(args[1]) THEN BEGIN
    module = 'PhotModS'
    ;help, /struct, kw
    PhotModS, args[0], args[1], status=status, _Extra=kw
    IF status NE 0 THEN BEGIN
      PRINT, 'Error in module: ', module
      exit, status=1
    ENDIF
  ENDIF
  
  ; Run PhotCorrS if given the pciof output filename
  IF nargs GT 2 THEN BEGIN
    module = 'PhotCorrS'
    ;help, /struct, kw
    PhotCorrS, args[0], args[2], args[1], status=status, _Extra=kw
    IF status NE 0 THEN BEGIN
      PRINT, 'Error in module: ', module
      exit, status=2
    ENDIF
  ENDIF
  
  ; Run BoloAlbedoS if given the bond output filename
  IF nargs GT 3 THEN BEGIN
    module = 'BoloAlbedoS'
    ;help, /struct, kw
    BoloAlbedoS, args[0], args[3], args[1], status=status, _Extra=kw
    IF status NE 0 THEN BEGIN
      PRINT, 'Error in module: ', module
      exit, status=3
    ENDIF
  ENDIF
    
  exit, status=0
end
