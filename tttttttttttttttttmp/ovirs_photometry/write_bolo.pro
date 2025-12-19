;+
; NAME:
;
;  write_bolo.pro
;
; PURPOSE:
;
;  Write bolometric Bond albedo to output file
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  write_bolo, outfile, boloalbedo, info[, error=error][, prop=prop]
;                                       [, sis=bolosis][, /overwrite]
;
; INPUTS:
;
;  outfile    - A string of the name of output file
;  boloalbedo - An array of bolometric bond albedo to be saved
;  info       - Information structure
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  error      - An array of the same shape as `boloalbedo`, the errors
;               of bolometric Bond albedo to be saved
;  prop       - A structure containing the header values to be propagated
;               to output file
;  sis        - SIS table file
;  overwrite  - If set, then the output will be overwritten if exist
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
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
; MODIFICATION HISTORY:
;
;  v1.0 : July 14, 2016, created by JYL @PSI
;  v1.1 : July 21, 2016, modified by JYL @PSI
;    * Changed `info' keyword to an input parameter
;    * Added `prop' keyword to propagate the values of FITS headers
;    * Revised program such that the output complies with SIS
;  v1.1.1 : March 16, 2017, JYL
;    * Add quality flags in `flag`, save to column 'BOND_ALBEDO_ERROR'
;      in the output
;  March 27, 2017, JYL @PSI
;    Change keyword `verbose` to `quiet`
;  April 7, 2017, JYL @PSI
;    Use system variable !quiet to set default keyword value `quiet`
;  May 4,2017, ZXD @PSI
;    Comment out the third HDU part which coursed fits verify problem.
;-

PRO write_bolo, outfile, bba, flag, info, error=err, prop=prop, sis=sisfile, overwrite=overwrite, quiet=quiet

    IF NOT keyword_set(quiet) THEN quiet=!quiet
    IF NOT keyword_set(err) THEN err=bba*0.

    ; load SIS
    sis_keys = load_sis(sisfile, quiet=quiet)

    ; primary extension
    mkhdr, hdr, 0, /extend
    keys = sis_keys.ext0
    FOR j=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[j], keys.val[j], keys.comm[j]
    ; update keyword values
    IF keyword_set(prop) THEN BEGIN
        tags = tag_names(prop)
        FOR i=0, n_elements(tags)-1 DO fxaddpar, hdr, tags[i], prop.(i)
    ENDIF
    mwrfits, dummy, outfile, hdr, create=overwrite, silent=quiet

    ; binary table header
    hdr = []
    fxbhmake, hdr, n_elements(struct)
    fxaddpar, hdr, 'NAXIS1', 158
    keys = sis_keys.ext1
    FOR j=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[j], keys.val[j], keys.comm[j]

    ; tag names for bolo albedo columns
    colnames = []
    FOR i=0, n_elements(keys.key)-1 DO IF strpos(keys.key[i],'TTYPE') NE -1 THEN colnames = [colnames, keys.val[i]]
    ncol = n_elements(colnames)
    albkey = colnames[-2]
    errkey = colnames[-1]

    ; generate info structure to be stored in a binary fits table
    tags = tag_names(info)
    ntags = n_elements(tags)
    nrow = n_elements(info.(0))
    struct = {}
    indices = intarr(ncol-2)
    ; add fields specified in the SIS first
    FOR i=0, ncol-3 DO BEGIN
        indices[i] = where(strmatch(tags, colnames[i], /fold_case) EQ 1)
        IF indices[i] NE -1 THEN BEGIN
            struct = create_struct(struct, colnames[i], info.(indices[i])[0])
            ;remove, indices[i], tags
        ENDIF ELSE message, 'Field "'+colnames[i]+'" not found in meta data'
    ENDFOR
    struct = create_struct(struct, albkey, 0., errkey, 0.)
    ; add the rest of fields propagated from `info` if any
    IF ntags GT ncol-2 THEN BEGIN
        tags1 = tags
        remove, indices, tags1
        n1 = n_elements(tags1)
        indices1 = intarr(n1)
        FOR i=0, n1-1 DO BEGIN
            indices1[i] = where(strmatch(tags, tags1[i], /fold_case))
            struct = create_struct(struct, tags1[i], info.(indices1[i])[0])
        ENDFOR
    ENDIF
    struct = replicate(struct, nrow)
    ; propagate values
    FOR i=0, ncol-3 DO struct.(i) = info.(indices[i])
    struct.(ncol-2) = bba
    struct.(ncol-1) = err
    IF ntags GT ncol-2 THEN FOR i=0, n1-1 DO struct.(i+ncol) = info.(indices1[i])

    ; adjust 'sclk' to the format specified in header
    fmt_index = strmid(keys.key[where(strmatch(keys.val,'sclk',/fold_case))],5)
    fmt = keys.val[where(strmatch(keys.key, 'TFORM'+fmt_index))]
    fmt = strmid(fmt, 0, strpos(fmt, 'A'))
    ik = where(strmatch(tags, 'sclk', /fold_case))
    FOR i=0, n_elements(info.(ik))-1 DO struct[i].(ik) = string(info.(ik)[i],format='(a'+fmt+')')

    ; save data
    mwrfits, struct, outfile, hdr, silent=quiet

END
