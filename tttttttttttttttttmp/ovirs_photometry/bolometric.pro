;+
; NAME:
;
;  bolometric.pro
;
; PURPOSE:
;
;  Calculate bolometric quantity
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  bolo = bolometric(q, wave, solar=solar)
;
; INPUTS:
;
;  q          - An array of quantity for which the bolometric quantity will be
;               calculated
;  wave       - Wavelength of `q' in nm
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  solar      - An array of Nx2, where solar[*,0] is wavelength in nm, and
;               solar[*,1] is the solar flux in W/[m2 nm].  If not specified,
;               then program will load it from solar_flux.txt in the current
;               working directory by default.
;  error      - An array of the same size as `q` for the errors of `q`.
;
; OUTPUTS:
;
;  Returns the bolometric quantity
;
; OPTIONAL OUTPUTS:
;
;  boloerr    - A named variable returns the error of returned bolometric
;               quantity.  Defined only when `error` is supplied.
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
;  v1.0.1 : March 13, 2017, created by JYL @PSI
;    Add keyword /nan to tsum call.
;    Call read_solar to load solar spectrum
;-

FUNCTION weighted_avg, q, w, error=e, avg_err=err
    ; q  - quantity to be averaged
    ; w  - weight
    ; error  - error of q
    ; avg_err  - returns the error of weighted average
    ;
    ; q, w, error must have the same number of elements
    idx = where(finite(q) AND finite(w))
    IF keyword_set(e) THEN idx = where(finite(q) AND finite(w) AND finite(e))
    IF idx[0] NE -1 THEN BEGIN
        wtot = total(w[idx])
        avg = total(q[idx]*w[idx])/wtot
        IF keyword_set(e) THEN BEGIN
            err = total((q[idx]+e[idx])*w[idx]) - total((q[idx]-e[idx])*w[idx])
            err = err/(2*wtot)
        ENDIF
    ENDIF ELSE BEGIN
        avg = 0.
        err = 0.
    ENDELSE

    RETURN, avg
END

FUNCTION bolometric, q, wave, solar=solar, error=e, boloerr=boloerr

    IF NOT keyword_set(solar) THEN BEGIN
        read_solar, 'solar_flux.txt', swv, flux
    ENDIF ELSE BEGIN
        swv = solar[*,0]
        sflx = solar[*,1]
    ENDELSE

    flx = interpol(sflx, swv, wave)
    sz = size(q,/dim)
    IF n_elements(sz) EQ 2 THEN BEGIN
        bolo = fltarr(sz[1])
        IF keyword_set(e) THEN BEGIN
            boloerr = fltarr(sz[1])
            FOR i=0, sz[1]-1 DO BEGIN
                bolo[i] = weighted_avg(q[*,i],flx,error=e[*,i],avg_err=ae)
                boloerr[i] = ae
            ENDFOR
        ENDIF ELSE FOR i=0, sz[1]-1 DO bolo[i] = weighted_avg(q[*,i],flx)
    ENDIF ELSE bolo = weighted_avg(q, flx, error=e, avg_err=boloerr)

    RETURN, bolo

END
