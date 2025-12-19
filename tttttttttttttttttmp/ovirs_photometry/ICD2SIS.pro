;+
; NAME:
;
;  ICD2SIS.pro
;
; PURPOSE:
;
;  Transfer ICD fits to SIS fits.
;
; CATEGORY:
;
;  Other tools
;
; CALLING SEQUENCE:
;
;  ICD2SIS, file_path, outfile_name, [, /overwrite][, /verbose]
;
;
; INPUTS:
;  file_path     - Path of the fits files
;  outfile_name    - A string specifying the output FITS file name
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  overwrite  - If set, then the output file will be overwritten if
;               already exists.
;  verbose    - Verbose mode
;
; OUTPUTS:
;
;  None
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
; PROCEDURE USED:
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : May 2, 2017, created by XDZ @PSI
;
;-

PRO ICD2SIS, infile, outfile, overwrite=overwrite, quiet=quiet, filter_invalid=filter_invalid

  if not keyword_set(quiet) then quiet=!quiet
  if size(filter_invalid,/type) eq 0 then filter_invalid=1

  data = read_spec(infile, error=error, wav=wav, info=info, header=header, quiet=quiet)

  write_spec, outfile, data, error, wav, info, /overwrite, quiet=quiet, sis='specdata_sis.csv'

END

