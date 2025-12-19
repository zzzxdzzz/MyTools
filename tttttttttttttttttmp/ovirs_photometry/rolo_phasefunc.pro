;+
; NAME:
;
;  rolo_phasefunc.pro
;
; PURPOSE:
;
;  Calculate the phase function for ROLO model
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = rolo_phasefunc(pha, c0, c1, a0, a1, a2, a3, a4[, /deriv])
;
; INPUTS:
;
;  pha    - Phase angle in degrees
;  c0, c1, a0, a1, a2, a3, a4 - Phase function parameters
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
;  derivatives w/r to the model parameters in a 2-D array of dimensions 8xM,
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
;  ROLO model:
;      f = c0*exp(-c1*pha) + a0 + a1*pha + a2*pha^2 + a3*pha^3 + a4*pha^4
;  Note that this phase function is not automatically normalized to unity at
;  zero degree phase angle.
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 15, 2017, created by JYL @PSI
;-

FUNCTION rolo_phasefunc, pha, c0, c1, a0, a1, a2, a3, a4, deriv=deriv
 f= 10 ^ (c1 * pha / 2.5); * 2/(1+cos(pha* !dtor))



; phase function
  ;  expterm = exp(-c1 * pha)
 ;   f = c0 * expterm + poly(pha, [a0, a1, a2, a3, a4])
    ; derivatives
 ;   IF keyword_set(deriv) THEN BEGIN
 ;       pha2 = pha*pha
 ;       J = [[expterm], $
 ;            [-c0*pha*expterm], $
 ;            [replicate(1.,n_elements(pha))], $
 ;            [pha], $
 ;            [pha2], $
 ;            [pha2*pha], $
 ;            [pha2*pha2]]
 ;       f = [transpose(f), transpose(j)]
 ;   ENDIF

    RETURN, f
END
