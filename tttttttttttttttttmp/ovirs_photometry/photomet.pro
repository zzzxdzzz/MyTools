;+
; NAME:
;
;  photomet.pro
;
; PURPOSE:
;
;  Apply photometric correction
;
; CATEGORY:
;
;  Photometric correction
;
; CALLING SEQUENCE:
;
;  corr_data = photomet(model, p, ref, pha, inc, emi[, refpha=refpha][, refinc=refinc][, refemi=refemi][, error=err][, _extra=_extra]
;
; INPUTS:
;
;  model      - A string to specify photometric model: 'lommel-seeliber',
;               'rolo', 'minnaert', 'mcewen' (case insensitive)
;  p          - Array, model parameter
;  ref, pha, inc, emi - Arrays, reflectance data, phase angles, incidence
;               angles, and emission angles (angles in degrees),
;               respectively
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  refpha, refinc, refemi - Arrays, reference angles for correction (all
;               in degrees).  Default is inc=30, emi=0, pha=30
;  error      - The error data associated with spectral data
;  _extra     - Extra keywords accepted by phot_model: /radf, /reff,
;               /brdf, to specify the quantity of input data `ref'.
;
; OUTPUTS:
;
;  Return corrected reflectance data
;
; OPTIONAL OUTPUTS:
;
;  error      - Keyword returns the photoemtrically corrected error data.
;               Returned error is simply the photometrically corrected input
;               error.
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
;  phot_model.pro
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : June 29, 2016, created by JYL @PSI
;-

FUNCTION photomet, model, p, ref, pha, inc, emi, refpha=refpha, refinc=refinc, refemi=refemi, error=err, _extra=_extra

    IF size(refpha, /type) EQ 0 THEN refpha = 30.
    IF size(refinc, /type) EQ 0 THEN refinc = 30.
    IF size(refemi, /type) EQ 0 THEN refemi = 0.
    npha = n_elements(refpha)
    ninc = n_elements(refinc)
    nemi = n_elements(refemi)
    IF (npha NE ninc) OR (npha NE nemi) THEN message, 'Error: `refpha`, `refinc`, and `refemi` do not have the same numbers of elements'
    ref1 = ref[*]  ; flatten input `ref` data

    ; photometric correction to input data
    iofs = phot_model(model, [refpha,pha], [refinc,inc], [refemi,emi], p, _extra=_extra)
    iof_ref = iofs[0:npha-1]
    iof_model = iofs[npha:*]

    ; calculate corrected reflectance quantity
    corr = []
    f_corr = []
    FOR k=0, npha-1 DO BEGIN
        factor = iof_ref[k] / iof_model
        f_corr = [[f_corr], [factor]]
        corr = [[corr], [ref1 * factor]]
    ENDFOR

    ; photometric correction error
    IF keyword_set(err) THEN err = transpose(err ## replicate(1,npha)) * f_corr

    RETURN, corr

END
