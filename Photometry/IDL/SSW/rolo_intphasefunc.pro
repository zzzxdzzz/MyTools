;+
; NAME:
;
;  rolo_intphasefunc.pro
;
; PURPOSE:
;
;  Calculate the disk-integrated phase function for ROLO model
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = rolo_intphasefunc(pha, c0, c1, a0, a1, a2, a3, a4[, /normalized][, /deriv][, perr=array][, err=var]))
;
; INPUTS:
;
;  pha    - Phase angle in degrees
;  c0, c1, a0, a1, a2, a3, a4 - Phase function parameters
;
; OPTIONAL INPUTS:
;
;  normalized - By default, the returned phase function will always be
;               normalized to unity at zero phase angle.  If this keyword
;               is explicitly set to 0, then the phase function is normalized
;               to geometric albedo at zero phase.
;  deriv  - If set, then program returns phase function and the derivatives
;           w/r to model parameters in a 2-D array.  Default is to calculate
;           model reflectance only
;  perr   - The uncertainty of phase function parameters.  It could be a 1-D
;           array containing the 1-sigma uncertainty, or a 2-D array of
;           covariance matrix.
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the disk-integrated phase function for ROLO model
;
;  If `deriv' is set, then program returns the model reflectance and
;  derivatives w/r to the model parameters in a 2-D array of dimensions
;  (N+1)xM, where N is the number of parameters, so [0,*] elements in the
;  return is the phase function and [1:*,*] is the derivatives.  Note that if
;  phase function is normalized then the derivatives will not include that
;  with respect to albedo parameter.
;
; OPTIONAL OUTPUTS:
;
;  err    - Returns the errors of phase function
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
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 15, 2017, created by JYL @PSI
;-

FUNCTION rolo_intphasefunc, pha, c0, c1, a0, a1, a2, a3, a4, normalized=normalized, deriv=deriv, perr=perr, err=err
    IF keyword_set(deriv) OR keyword_set(perr) THEN  $
        calc_deriv=1 ELSE calc_deriv=0

    ; calculate phase function and derivative if needed
    f = rolo_phasefunc(pha, c0, c1, a0, a1, a2, a3, a4, deriv=calc_deriv)
    f0 = rolo_phasefunc(0, c0, c1, a0, a1, a2, a3, a4)
    IF calc_deriv THEN BEGIN
        phi = replicate(1, (size(f))[1]) # ls_int(pha) * f
        phi = [phi[0,*], phi[0,*]/f0, phi[1:*,*]]
    ENDIF ELSE phi = ls_int(pha) * f

    ; normalization
    IF size(normalized,/type) EQ 0 THEN normalized=1
    IF normalized THEN BEGIN
        phi /= f0
        IF calc_deriv THEN phi = [phi[0,*], phi[2:*,*]]
    ENDIF

    ; calculate phase function error
    IF keyword_set(perr) THEN BEGIN
        npha = n_elements(pha)
        IF size(perr,/n_dim) EQ 1 THEN err = sqrt(total((perr # replicate(1,npha) * phi[1:*,*])^2,1))  $
        ELSE IF size(perr,/n_dim) EQ 2 THEN err = sqrt(total(phi[1:*,*] ## perr * phi[1:*,*],1))
    ENDIF

    IF keyword_set(perr) AND NOT keyword_set(deriv) THEN  $
        RETURN, reform(phi[0,*])  $
    ELSE RETURN, phi
END
