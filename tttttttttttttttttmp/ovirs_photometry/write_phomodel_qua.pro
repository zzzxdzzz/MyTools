;+
; NAME:
;
;  write_phomodel_qua.pro
;
; PURPOSE:
;
;  Save photometric models quality data to output FITS file.
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  write_phomodel_qua, model, qua, wav, outfile[, /overwrite]
;
; INPUTS:
;
;  model      - A string to specify what model to fit.  The options are
;               'lommel-seeliger', 'rolo', 'minnaert', mcewen' (case
;               insensitive).  If the specified model is not recognized, then
;               program will generate an error.
;  qua        - Model fitting quality array, Nx8, where N is the number of
;               models, and 8 columns are slope, correlation for
;               1. model vs. measure, 2. measure/model vs phase,
;               3. measure/model vs. incidence, 4. measure/model vs emission
;  wav        - Wavelength(s)
;  outfile    - A string of the name of output file
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
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
;  fxbhmake.pro, mwrfits.pro, fxaddpar.pro
;  All is in Goddard IDL Astronomy User's Library
;  (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : March 16, 2017, created by JYL @PSI
;  March 27, 2017, JYL @PSI
;    Change keyword `verbose` to `quiet`
;  April 7, 2017, JYL @PSI
;    Use system variable !quiet to set default keyword value `quiet`
;-

PRO write_phomodel_qua, model, qua, wav, outfile, overwrite=overwrite, quiet=quiet

    IF NOT keyword_set(quiet) THEN quiet=!quiet

    ; prepare data
    qua_struct = replicate({wav: 0., slope: 0., corr: 0., slope_pha: 0., corr_pha: 0.,  slope_inc: 0., corr_inc: 0., slope_emi: 0., corr_emi: 0.}, n_elements(wav))
    qua_struct.wav = wav
    FOR j=1, 8 DO qua_struct.(j)=qua[*,j-1]

    ; save to fits
    fxbhmake, hdr, n_elements(qua_struct)
    fxaddpar, hdr, 'EXTNAME', model
    mwrfits, qua_struct, outfile, hdr, create=overwrite, silent=quiet

END
