;+
; NAME:
;
;  mcewen_intphasefunc.pro
;
; PURPOSE:
;
;  Calculate the disk-integrated phase function for McEwen model
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = mcewen_intphasefunc(pha, p1, p2, p3, p4, p5, p6[, a_mc=1][, /deriv][, perr=array][, err=var]))
;
; INPUTS:
;
;  pha    - Phase angle in degrees
;  p1, p2, p3 - McEwen partition function parameters
;  p4, p5, p6 - Phase function parameters
;
; OPTIONAL INPUTS:
;
;  a_mc   - Geometric albedo.  Normally the returned phase function is
;           normalized to 1 at zero phase angle.  If `a_mc` is set, then the
;           phase function will be normalized to `a_mc` at zero phase angle.
;  deriv  - If set, then program returns phase function and the derivatives
;           w/r to model parameters in a 2-D array.  Default is to calculate
;           model reflectance only
;  perr   - The uncertainty of phase function parameters.  It could be a 1-D
;           array containing the 1-sigma uncertainty, or a 2-D array of
;           covariance matrix.  The size of array must match the number of
;           phase function parameters provided.  I.e., if `a_mc` is provided,
;           then `perr` must have a length of 7 with the first position
;           corresponding to `a_mc`.  If `a_mc` is not provided, then `perr`
;           has a length of 6.
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the disk-integrated phase function for McEwen model
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
;  Calculation follows Table 4 in Li et al. (2015) Asteroid IV photometry
;  chapter, although there is a mistake there.
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 15, 2017, created by JYL @PSI
;-

FUNCTION mcewen_intphasefunc, pha, p1, p2, p3, p4, p5, p6, a_mc=a_mc, deriv=deriv, perr=perr, err=err
    IF keyword_set(deriv) OR keyword_set(perr) THEN  $
        calc_deriv=1 ELSE calc_deriv=0

    ; calculate phase function and derivative if needed
    Lalpha = expoly(pha, [0, p1, p2, p3], deriv=calc_deriv)
    f = expoly(pha, [0, p4, p5, p6], deriv=calc_deriv)
    IF calc_deriv THEN BEGIN
       phi1 = (2.d/3 * (1-Lalpha[0,*]) * lambert_int(pha) + Lalpha[0,*] * ls_int(pha))
        phi = phi1 * f[0,*]
        dphidf = replicate(1,3) # phi1 * f[2:*,*]
        dphidL = replicate(1,3) # ((-2.d/3 * lambert_int(pha) + ls_int(pha)) * f[0,*]) * Lalpha[2:*,*]
        phi = [phi, dphidL, dphidf]
    ENDIF ELSE  $
       phi = (2.d/3 * (1-Lalpha) * lambert_int(pha) + Lalpha * ls_int(pha)) * f

    ; albedo scaling
    IF keyword_set(a_mc) THEN BEGIN
        phi0 = phi[0,*]
        phi *= a_mc
        IF calc_deriv THEN phi = [phi[0,*], phi0, phi[1:*,*]]
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
