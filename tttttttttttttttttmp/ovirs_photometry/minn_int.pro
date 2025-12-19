;+
; NAME:
;
;  minn_int.pro
;
; PURPOSE:
;
;  Calculate Minnaert integrated term with numerical interpolation
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = minn_int(pha, k[, cubic=[-1, 0]][, /deriv])
;
; INPUTS:
;
;  pha     - Phase angle in degrees
;  k       - Minnaert k parameter
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  cubic   - See IDL `interpolate`
;  deriv   - If set, then return the integrated term and partial derivative
;            with respect to parameter `k` in a Nx2 array.
;
; OUTPUTS:
;
;  Returns the Minnaert integrated term
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
;  Program calculates integrated term by interpolating over the tabulated
;  values of Minnaert integrated function from numerical calculation
;  `diskint('minnaert', findgen(181), k, nstep=1000)` with k in [0.4, 1.2] and
;  saved in 'minn_int.fits'.
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  09/21/2018, created by JYL @PSI
;-

FUNCTION minn_int, pha, k, cubic=cubic, deriv=deriv

    findpro, 'minn_int', /noprint, dirlist=d
    minn = readfits(d[0]+'minn_int.fits', /silent)
    pha0 = readfits(d[0]+'minn_int.fits', ext=2, /silent)
    npha = n_elements(pha0)
    k0 = readfits(d[0]+'minn_int.fits', ext=3, /silent)
    nk = n_elements(k0)
    p_pha = (pha-min(pha0)) / (max(pha0)-min(pha0)) * (npha-1)
    p_k = (k-min(k0)) / (max(k0)-min(k0)) * (nk-1)
    out = interpolate(minn, p_pha, p_k, cubic=cubic)
    IF keyword_set(deriv) THEN BEGIN
        der = readfits('minn_int.fits', ext=1, /silent)
        out_der = interpolate(der, p_pha, p_k, cubic=cubic)
        np = n_elements(out_der)
        out = transpose([[reform(out, np)], [reform(out_der, np)]])
    ENDIF
    RETURN, out

END
