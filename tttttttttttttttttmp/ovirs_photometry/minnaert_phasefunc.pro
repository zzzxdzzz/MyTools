;+
; NAME:
;
;  minnaert_phasefunc.pro
;
; PURPOSE:
;
;  Calculate the phase function used for Minnaert model
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  f = minnaert_phasefunc(pha, p1, p2, p3[, /deriv])
;
; INPUTS:
;
;  pha        - Phase angle in degrees
;  p1, p2, p3 - Phase function parameters
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  deriv  - If set, then program returns phase function and the derivatives
;           w/r to model parameters in a 2-D array.  Default is to calculate
;           model reflectance only
;
; OUTPUTS:
;
;  By default, program returns the reflectance quantity for the input
;  photometric parameters and geometries with the specified model in a 1-D
;  array of M elements, where M is number of phase angles.
;
;  If `deriv' is set, then program returns the model reflectance and
;  derivatives w/r to the model parameters in a 2-D array of dimensions 4xM,
;  where [0,*] is the phase function and [1:*,*] is the derivatives.
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
;  Phase function used in Minnaert model
;      falpha = 10^(-0.4 * (p1 * pha + p2 * pha^2 + p3 * pha^3))
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 15, 2017, created by JYL @PSI
;-

FUNCTION minnaert_phasefunc, pha, p1, p2, p3, deriv=deriv

    f = 10^(-0.4 * poly(pha, [0, p1, p2, p3]))
    ; derivatives
    IF keyword_set(deriv) THEN BEGIN
        j = [[pha], [pha*pha], [pha*pha*pha]]
        ; -0.4*log(10) = -0.92103405092209500
        j *=  f # replicate(-0.92103405092209500,3)
        f = [transpose(f), transpose(j)]
    ENDIF

    RETURN, f
END
