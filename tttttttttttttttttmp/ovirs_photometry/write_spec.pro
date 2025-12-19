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
;                                             [, /overwrite][, /quiet]
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
;  quiet    - quiet mode
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
;  findpro.pro
;  mkhdr.pro, fxbhmake.pro, fxaddpar.pro, mwrfits.pro
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
;  v2.0 : March 8, 2017, updated by JYL @PSI
;     * Major update with the updated load_sis.pro
;  March 27, 2017, JYL @PSI
;    Change keyword `verbose` to `quiet`
;  April 7, 2017, JYL @PSI
;    Use system variable !quiet to set default keyword value `quiet`
;-

PRO write_spec, outfile, data, err, wav, info, prop=prop, overwrite=overwrite, sis=sisfile, quiet=quiet

    IF NOT keyword_set(sisfile) THEN BEGIN
       findpro, 'write_spec', /noprint, dirlist=d
       sisfile = d[0]+'/specdata_sis.csv'
    ENDIF

    IF NOT keyword_set(quiet) THEN quiet=!quiet

    ; generate header
    sis_keys = load_sis(sisfile, quiet=quiet)

    ; primary extension
    data = float(data)
    mkhdr, hdr, data, /extend
    keys = sis_keys.ext0
    FOR i=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[i], keys.val[i], keys.comm[i]
    ; Update some keyword values
    IF keyword_set(prop) THEN BEGIN
        tags = tag_names(prop)
        FOR i=0, n_elements(tags)-1 DO fxaddpar, hdr, tags[i], prop.(i)
    ENDIF
    mwrfits, data, outfile, hdr, create=overwrite, /silent

    ; second extension
    err = float(err)
    mkhdr, hdr, err, /image
    keys = sis_keys.ext1
    FOR i=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[i], keys.val[i], keys.comm[i]
    mwrfits, err, outfile, hdr, /silent

    ; third extension
    wav = float(wav)
    mkhdr, hdr, wav, /image
    keys = sis_keys.ext2
    FOR i=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[i], keys.val[i], keys.comm[i]
    mwrfits, wav, outfile, hdr, /silent

    ; fourth extension
    ; generate info structure to be stored in a binary fits table
    struct = {}
    tags = tag_names(info)
    FOR i=0, n_elements(tags)-1 DO struct = create_struct(struct, tags[i], info.(i)[0])
    struct = replicate(struct, n_elements(info.(0)))
    FOR i=0, n_elements(tags)-1 DO struct.(i) = info.(i)
    ; header
    fxbhmake, hdr, n_elements(struct)
    fxaddpar, hdr, 'NAXIS1', 134
    keys = sis_keys.ext3
    FOR i=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[i], keys.val[i], keys.comm[i]

    ; adjust 'sclk' to the format specified in header
    fmt_index = strmid(keys.key[where(strmatch(keys.val,'sclk',/fold_case))],5)
    fmt = keys.val[where(strmatch(keys.key, 'TFORM'+fmt_index))]
    fmt = strmid(fmt, 0, strpos(fmt, 'A'))
    ik = where(strmatch(tags, 'sclk', /fold_case))
    FOR i=0, n_elements(info.(ik))-1 DO struct[i].(ik) = string(info.(ik)[i],format='(a'+fmt+')')

    mwrfits, struct, outfile, hdr, /silent

END
