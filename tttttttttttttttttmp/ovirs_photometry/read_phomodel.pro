;+
; NAME:
;
;  read_phomodel.pro
;
; PURPOSE:
;
;  Read photometric models from input FITS file.
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  p = read_phomodel(infile, model[, sis=conf][, parname=parname][, wav=wav]
;                                 [, header=header][, covar=covar][, /quiet]
;
; INPUTS:
;
;  infile     - A string specifying the input FITS file
;  model      - A string to specify what model to load.  The options are
;               'lommel-seeliger', 'rolo', 'minnaert', mcewen' (case
;               insensitive).  If the specified model is not recognized, then
;               program will generate an error.
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the photometric parameters and parameters uncertainty in an array
;
; OPTIONAL OUTPUTS:
;
;  wav        - Return wavelength(s)
;  parname    - Returns an array of string with the names of photometric
;               parameters
;  header     - Returns the header information
;  covar      - Returns the covariance matrix
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
;  All is in Goddard IDL Astronomy User's Library
;  (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : June 7, 2016, created by JYL @PSI
;  v1.1 : July 15, 2016, modified by JYL @PSI
;     Changed the call to `load_phomodel_sis' according to the new API of
;     `load_phomodel_sis'
;  v1.2 : July 20, 2016, modified by JYL @PSI
;     Added keyword `header'
;  v1.3 : March 6, 2017, modified by JYL @PSI
;     * Changed API to add a keyword `chisq` to return modeling chisq
;     * Improved the returned header
;  v1.4 : March 8, 2017, updated by JYL @PSI
;     * Use the updated load_sis.pro to load SIS
;  March 27, 2017, JYL @PSI
;    Change keyword `verbose` to `quiet`
;    Add error handling
;    Allow arbitrary order in photomoetric parameter file
;    Major change to use new photometric model data format
;  April 7, 2017, JYL @PSI
;    Set default `quiet` keyword to system variable !quiet
;  June 28, 2017, XDZ @PSI
;    Update output explaination, include parameters uncertainty
;-

FUNCTION read_phomodel, infile, model, sis=conf, parname=parname, wav=wav, chisq=chisq, header=header, covar=covar, quiet=quiet

    IF NOT keyword_set(quiet) THEN quiet=!quiet

    IF ~file_test(infile) THEN message, 'input file '+infile+' not found'

    ; load fits keywords from sis configuration file
    IF NOT keyword_set(conf) THEN BEGIN
       findpro, 'read_phomodel', /noprint, dirlist=d
       conf = d[0]+'/phopar_sis.csv'
    ENDIF
    sis_keys = load_sis(conf, quiet=quiet)

    ; create model indexes in the loaded sis_keys
    ngrp = n_elements(tag_names(sis_keys))
    mod_names = strarr(ngrp-1)
    FOR i=1, ngrp-1 DO BEGIN
        indx = where(strmatch(sis_keys.(i).key, 'extname', /fold_case))
        mod_names[i-1]=sis_keys.(i).val[indx]
    ENDFOR

    ; load data
    p = mrdfits(infile, model, h2, status=s, silent=quiet)
    indx = where(strmatch(mod_names, model, /fold_case))+1
    keys = sis_keys.(indx).key
    vals = sis_keys.(indx).val
    nc = vals[where(keys EQ 'NCOEFF')]
    nc = nc[0]
    parname = strarr(nc)
    FOR i=0, nc-1 DO parname[i] = vals[where(keys EQ 'COEFF'+string(i+1,format='(i1)'))]
    wav = p[*,0]
    chisq = p[*,1]
    p = p[*,2:*]
    ; process header
    h1 = headfits(infile, ext=0, silent=silent)
    w1 = where(strmatch(h1,'mission*',/fold_case) EQ 1)
    w2 = where(strmatch(h1,'extname*',/fold_case) EQ 1)
    h1 = h1[w1:w2-1]
    w1 = where(strmatch(h2,'extname*',/fold_case) EQ 1)
    h2 = h2[w1:*]
    header = [h1,h2]

    ; load covariance matrix
    covar = mrdfits(infile, model+'_Covar', status=s, silent=quiet)

    RETURN, p

END
