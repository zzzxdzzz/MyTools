;+
; NAME:
;
;  write_spec.pro
;
; PURPOSE:
;
;  Save spectral data file.
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  write_spec, outfile, data, error, wav, info[, prop=prop][, sis=sisfile]
;                                             [, /overwrite][, /verbose]
;
; INPUTS:
;
;  outfile    - A string specifying the output FITS file
;  data       - Spectral data array to be saved
;  err        - The error data associated with spectral data, to be
;               saved if passed.
;  info       - A structure containing the ancillary information of
;               spectral data.  The same as the output `info' from
;               read_spec.pro
;  wav        - Wavelengths of output data, to be saved if passed.
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  prop       - A structure containing the values of some FITS header keys
;               whose value should be updated.
;  sis        - SIS configuration .csv file for spectral data.
;               Default is 'specdata_sis.csv' in the same directory
;               as this procedure.
;  overwrite  - If set, then the output file will be overwritten if
;               already exists.
;  verbose    - Verbose mode
;  isolate_comments - Blank comment lines will be placed around 
;                     comments for better visibility
;  icd        - generate output using ICD format
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
;  findpro.pro, which in turn uses 'fdecomp.pro'
;  mkhdr.pro, sxaddpar.pro, mwrfits.pro
;  All is in Goddard IDL Astronomy User's Library
;  (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : June 29, 2016, created by JYL @PSI
;  v1.1 : July 15, 2016, modified by JYL @PSI
;     * Used `load_sis.pro' to load SIS file
;     * Slight optimization for `overwrite' parameter
;  v1.2 : July 20, 2016, modified by JYL @PSI
;     * Changed keywords `err', `wav', `info' to input parameters
;     * Add keyword `prop' to propagate values of keywords
;     * Revised the program so the output complies with the current SIS
;
;  2016/07/28:  Adapted from write_spec.  jj@cornell
;-

PRO jj_write_spec, outfile, data, err, wav, info, prop=prop, overwrite=overwrite, sis=sisfile, $
                   verbose=verb, isolate_comments=isolate_comments, icd=icd

  IF NOT keyword_set(verb) THEN verb=0

  ;; format data for ICD
  if keyword_set(icd) then begin
    ;; fake quality data
    quality = data
    quality[*] = 1

    ;; 3D data/uncertainty/quality array
    data_use = transpose([[[data]],[[err]],[[quality]]],[0,2,1])
  endif else begin
    data_use = data
  endelse

  ;; generate header
  head = load_sis(sisfile, data_use, verbose=verb, isolate_comments=isolate_comments)

  ;; Update some keyword values
  IF keyword_set(prop) THEN BEGIN
    tags = tag_names(prop)
    FOR i=0, n_elements(tags)-1 DO sxaddpar, head, tags[i], prop.(i)
  ENDIF

  if not keyword_set (icd) then begin
    ;; generate header info for uncertainty data
    tmp_head = load_sis(sisfile, info, h2=h_unc, verbose=verb, isolate_comments=isolate_comments, xt=1)
  endif
  ;; generate header info for wavelength data
  tmp_head = load_sis(sisfile, info, h2=h_wvl, verbose=verb, isolate_comments=isolate_comments, xt=2)
  ;; geenerate header info for binary table data
  tmp_head = load_sis(sisfile, info, h2=h_bin, verbose=verb, isolate_comments=isolate_comments, xt=3)

  ;; save data
  mwrfits, data_use, outfile, head, create=overwrite, silent=~verb
  if not keyword_set(icd) then mwrfits, err, outfile, h_unc, silent=~verb
  mwrfits, wav, outfile, h_wvl, silent=~verb
  mwrfits, info, outfile, h_bin, silent=~verb, /no_comment

END
