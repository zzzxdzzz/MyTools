;+
; NAME:
;
;  write_phomodel.pro
;
; PURPOSE:
;
;  Save photometric models to output FITS file.
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  write_phomodel, model, p, covar, wav, outfile[, prop=prop][, sis=conf]
;                                        [, /overwrite][, /quiet]
;
; INPUTS:
;
;  model      - A string to specify what model to fit.  The options are
;               'lommel-seeliger', 'rolo', 'minnaert', mcewen' (case
;               insensitive).  If the specified model is not recognized, then
;               program will generate an error.
;  p          - Photometric model parameters and their uncertainty in an array
;  covar      - Covariance matrix of the input parameters
;  wav        - Wavelength(s)
;  chisq      - Model chi-square
;  outfile    - A string of the name of output file
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  prop       - A structure containing the values of some FITS header keys
;               whose value should be updated.
;  sis        - A string of the name of the SIS configuration file.  Default is
;               'phopar_sis.csv' in the same directory as this program file.
;               If configuration file not found, program will generate an
;               error.
;  overwrite  - If set, then if the output file exists, it will be overwritten.
;               Default is to append an extension to any existing output file.
;
; OUTPUTS:
;
;  None
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
;  findpro.pro, which in turn uses 'fdecomp.pro'
;  mkhdr.pro, fxaddpar.pro, writefits.pro
;  All is in Goddard IDL Astronomy User's Library
;  (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : May 24, 2016, created by JYL @PSI
;  v1.1 : June 7, 2016, modified by JYL @PSI
;     * Stripped out load_sis.pro to a separate file for use by
;       read_phomodel.pro, and changed the rubroutine name to
;       load_phomodel_sis.pro
;     * Change keyword `configuration' to `sis'
;  v1.2 : July 15, 2016, modified by JYL @PSI
;     Changed the call to `load_phomodel_sis' according to the new API of
;     `load_phomodel_sis'
;  v1.3 : July 20, 2016, modified by JYL @PSI
;     * Added parameter `prop' to propogate some configuration parameters to
;       the output file
;     * Filter out 'comment' line in the _sis.csv files.  TBD: properly process
;       comment lines.
;  v1.4 : March 6, 2017, modified by JYL @PSI
;     * Changed the API to add `chisq` as an explicite input parameter
;  v1.5 : March 8, 2017, updated by JYL @PSI
;     * Use new load_sis.pro instead of load_phomodel_sis.pro
;  March 27, 2017, JYL @PSI
;    Change keyword `verbose` to `quiet`
;    Major change to use new photometric model file format in order to increase
;      flexibility in reading and writing
;  April 7, 2017, JYL @PSI
;    Use system variable !quiet to set default keyword value `quiet`
;  July 4, 2017, XDZ @PSI
;    Add input 'covar'--covariance matrix of parameters for file saving,
;    Save covar as a extra extension after parameters
;-

PRO write_phomodel, model, p, covar, wav, chisq, outfile, prop=prop, sis=sis, overwrite=overwrite, quiet=quiet

    IF NOT keyword_set(quiet) THEN quiet=!quiet

    ; prepare data
    w1 = reform(wav,1,n_elements(wav))
    c1 = reform(chisq,1,n_elements(chisq))
    p1 = transpose(p)
    data = transpose([w1, c1, p1])

    ; load sis keys
    IF NOT keyword_set(sis) THEN BEGIN
       findpro, 'write_phomodel', /noprint, dirlist=d
       sis = d[0]+'/phopar_sis.csv'
    ENDIF
    sis_keys = load_sis(sis, quiet=quiet)
    ; generate primary fits header
    IF ~file_test(outfile) OR keyword_set(overwrite) THEN BEGIN
        mkhdr, hdr, '', /extend
        keys = sis_keys.ext0
        FOR j=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[j], keys.val[j], keys.comm[j]
        IF keyword_set(prop) THEN BEGIN
            tags = tag_names(prop)
            FOR j=0, n_elements(tags)-1 DO fxaddpar, hdr, tags[j], prop.(j)
        ENDIF
        writefits, outfile, 0, hdr
    ENDIF

    ; load fits keywords from sis configuration file
    ; load header for parameter extersion
    i = 0
    n_sis_tags = n_tags(sis_keys)
    REPEAT i+=1 UNTIL (i GE n_sis_tags) OR  $
            ((where(strmatch(sis_keys.(i).val, model, /fold_case)))[0] NE -1)
    keys = sis_keys.(i)
    mkhdr, hdr, data, /image
    FOR j=0, n_elements(keys.key)-1 DO fxaddpar, hdr, keys.key[j], keys.val[j], keys.comm[j], /nological
    IF total(p NE 0)+total(covar NE 0) LT 0.5 THEN fxaddpar, hdr, 'FITTED', 'NO' $
    ELSE fxaddpar, hdr, 'FITTED', 'YES'

    ; fix integer type values
    ww = (where(strlowcase(keys.key) EQ 'ncoeff'))[0]
    IF ww GE 0 THEN fxaddpar, hdr, keys.key[ww], fix(keys.val[ww])
    ; update values of a few keywords
    fxaddpar, hdr, 'wave', wav[0]

    ; load header for covarience extension
    i = 0
    n_sis_tags = n_tags(sis_keys)
    REPEAT i+=1 UNTIL (i GE n_sis_tags) OR  $
            ((where(strmatch(sis_keys.(i).val, model+'_Covar', /fold_case)))[0] NE -1)
    keys = sis_keys.(i)
    mkhdr, covar_hdr, covar, /image
    FOR j=0, n_elements(keys.key)-1 DO fxaddpar, covar_hdr, keys.key[j], keys.val[j], keys.comm[j]

    ; save output file
    writefits, outfile, data, hdr, /append
    writefits, outfile, covar,covar_hdr ,/append


END
