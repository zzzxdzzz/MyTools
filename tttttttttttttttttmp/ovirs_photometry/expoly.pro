;+
; NAME:
;
;  expoly.pro
;
; PURPOSE:
;
;  Calculate exponential polynomial function
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  f = expoly(pha, par[, /deriv])
;
; INPUTS:
;
;  pha    - Phase angle in degrees
;  par    - Parameters
;
; OPTIONAL INPUTS:
;
;  deriv  - If set, then program returns phase function and the derivatives
;           w/r to model parameters in a 2-D array.  Default is to calculate
;           model reflectance only
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  By default, program returns the reflectance quantity for the input
;  photometric parameters and geometries with the specified model in a 1-D
;  array of M elements, where M is number of phase angles.
;
;  If `deriv' is set, then program returns the model reflectance and
;  derivatives w/r to the model parameters in a 2-D array of dimensions
;  (N+1)xM, where N is the number of parameters, so [0,*] elements in the
;  return is the phase function and [1:*,*] is the derivatives.
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
;  The exponential polynomial function p at specified phase angle:
;      f = exp(p[0] + p[1] * pha + p[2] * pha^2 + p[3] * pha^3 + ...)
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 19, 2017, created by JYL @PSI
;-

FUNCTION expoly, pha, par, deriv=deriv

    f = exp(poly(pha, par))
    ; derivatives
    npar = n_elements(par)
    IF keyword_set(deriv) THEN BEGIN
        pha_nth = replicate(1.,n_elements(pha))
        j = []
        FOR i=0,npar-1 DO BEGIN
            j = [[j], [[pha_nth]]]
            pha_nth *= pha
        ENDFOR
        j *= f#replicate(1.,npar)
        f = [transpose(f), transpose(j)]
    ENDIF

    RETURN, f
END
