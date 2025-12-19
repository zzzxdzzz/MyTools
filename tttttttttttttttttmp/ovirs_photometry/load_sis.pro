;+
; NAME:
;
;  load_sis.pro
;
; PURPOSE:
;
;  Load SIS file and return keywords content
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  sis_keys = load_sis(sisfile[, /quiet])
;
; INPUTS:
;
;  sisfile    - A string specifying the input SIS file
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  quiet    - quiet mode
;
; OUTPUTS:
;
;  Returns a structure with fields .ext0, .ext1, .ext2, ..., each containing
;  the keys of the primary, first extension, second extension, ...
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; SIDE EFFECTS:
;
;
; RESTRICTIONS:
;
;
; PROCEDURE USED:
;
;  readcol.pro
;  Available in Goddard IDL Astronomy User's Library
;  (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : July 15, 2016, created by JYL @PSI
;  v1.1 : July 20, 2016, modified by JYL @PSI
;    Filter out 'comment' line in the _sis.csv files.  TBD: properly process
;    comment lines.
;  v1.2 : July 21, 2016, modified by JYL @PSI
;    Added capability to generate headers for binary table data
;  v1.3 : July 25, 2016, modified by JYL @PSI
;    Used size(data, /type) rather than type(data) to find out the type of data
;  v2.0 : March 8, 2017, updated by JYL @PSI
;    Major update including API change, to return a structure containing the
;    keys of multiple FITS extensions.  This also requires some slight changes
;    in the format of SIS files.
;  v2.1 : March 16, 2017, updated by ZXD @PSI
;    Change the undefined function INT to FIX.
;  March 27, 2017, JYL @PSI
;    Use system variable !quiet and `message` to control info message
;    Change keyword `verbose` to `quiet`
;  April 7, 2017, JYL @PSI
;    Use `print` rather than `message` to print out information messages
;-

FUNCTION load_sis, sisfile, quiet=quiet

    IF NOT keyword_set(quiet) THEN quiet=!quiet
    IF ~quiet THEN print, 'Load SIS from '+sisfile
    IF NOT file_test(sisfile) THEN message, 'SIS file '+sisfile+' not found'

    ; populate header keywords
    readcol, sisfile, key, val, comm, format='a,a,a', delimiter=',', /silent, stringskip='Comment'
    n_ext = val[0]
    key_start = intarr(n_ext)
    FOR i=0, n_ext-1 DO BEGIN
        IF i LT 9 THEN key_start[i] = where(strmatch(key, string(i+1, format='(i1)')) EQ 1)+1 $
        ELSE key_start[i] = where(strmatch(key, string(i+1, format='(i2)')) EQ 1)+1
    ENDFOR
    key_end = [key_start[1:*]-2, n_elements(key)-1]
    sis_keys = {}
    FOR i=0, n_ext-1 DO BEGIN
        ext_key = create_struct('key', key[key_start[i]:key_end[i]], 'val', val[key_start[i]:key_end[i]], 'comm', comm[key_start[i]:key_end[i]])
        IF i LT 9 THEN sis_keys = create_struct(sis_keys, 'ext'+string(i, format='(I1)'), ext_key) $
        ELSE sis_keys = create_struct(sis_keys, 'ext'+string(i, format='(I2)'), ext_key)
    ENDFOR

    IF ~quiet THEN print, 'Loaded '+string(fix(total(key_end-key_start+1)))+' keys in '+string(n_ext)+' extensions successfully.'
    RETURN, sis_keys

END
