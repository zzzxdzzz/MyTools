;+
; NAME:
;
;  combine_fits.pro
;
; PURPOSE:
;
;  Combine IPDIF fits files.
;
; CATEGORY:
;
;  Other tools
;
; CALLING SEQUENCE:
;
;  combine_fits, file_path, outfile_name, [, /overwrite][, /verbose]
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
;  v1.0 : Dec 18, 2016, created by XDZ @PSI
;  v1.0.1 : Feb 2, 2017, modified by JYL @PSI
;    Fix a bug in generating output file name string
;  v1.0.2 : Mar 8, 2017, modified by JYL @PSI
;    * Propagate all fields in binary tables
;    * Use write_spec.pro to save output data
;-

PRO combine_fits, file_names, outfile, overwrite=overwrite, quiet=quiet, filter_invalid=filter_invalid

  if not keyword_set(quiet) then quiet=!quiet
  if size(filter_invalid,/type) eq 0 then filter_invalid=1
  data0=[]
  error0=[]
  sclk0=[]

  lat0=[]
  lon0=[]
  inc0=[]
  emi0=[]
  pha0=[]
  ffac0=[]
  range0=[]
  for i = 0, n_elements(file_names)-1 do begin
    data = read_spec(file_names[i], error=error, wav=wav, info=info, header=header, quiet=quiet)
    data0=[[data0],[data]]
    error0=[[error0],[error]]
    if i eq 0 then info0=info else begin
      info_new = {}
      tags = tag_names(info)
      for j=0, n_elements(tags)-1 do info_new = create_struct(info_new, tags[j], [info0.(j), info.(j)])
      info0 = info_new
    endelse
  endfor

  ; filter out invalid data points
  if filter_invalid then begin
    good = where((info0.lat ne 0) and (info0.lon ne 0) and (info0.incidang ne 0) and (info0.emissang ne 0) and (info0.phaseang ne 0))
    data0 = data0[*,good]
    error0 = error0[*,good]
    info_new = {}
    tags = tag_names(info)
    for i=0, n_tags(info0)-1 do info_new = create_struct(info_new, tags[i], info0.(i)[good])
    info0 = info_new
  endif

  write_spec, outfile, data0, error0, wav, info0, /overwrite, quiet=quiet, sis='specdata_sis.csv'

END

