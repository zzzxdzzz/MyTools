;+
; NAME:
;
;  ls_intphasefunc.pro
;
; PURPOSE:
;
;  Calculate the disk-integrated phase function for Lommel-Seeliger model
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = ls_intphasefunc(pha, p1, p2, p3[, a_ls=1][, /deriv][, perr=array][, err=var])
;
; INPUTS:
;
;  pha        - Phase angle in degrees
;  p1, p2, p3 - Phase function parameters
;
; OPTIONAL INPUTS:
;
;  a_ls   - Lommel-Seeliger albedo.  Normally the returned phase function is
;           normalized to 1 at zero phase angle.  If `a_ls` is set, then the
;           phase function will be normalized to `a_ls` at zero phase angle.
;  deriv  - If set, then program returns phase function and the derivatives
;           w/r to model parameters in a 2-D array.  Default is to calculate
;           model reflectance only
;  perr   - The uncertainty of phase function parameters.  It could be a 1-D
;           array containing the 1-sigma uncertainty, or a 2-D array of
;           covariance matrix.  The size of array must match the number of
;           phase function parameters provided.  I.e., if `a_ls` is provided,
;           then `perr` must have a length of 4 with the first position
;           corresponding to `a_ls`.  If `a_ls` is not provided, then `perr`
;           has a length of 3.
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the disk-integrated phase function for Lommel-Seeliger model
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

FUNCTION ls_intphasefunc, pha, p1, p2, p3, a_ls=a_ls, deriv=deriv, perr=perr, err=err
    IF keyword_set(deriv) OR keyword_set(perr) THEN  $
        calc_deriv=1 ELSE calc_deriv=0

    ; calculate phase function and derivative if needed
    f = expoly(pha, [0, p1, p2, p3], deriv=calc_deriv)
    IF calc_deriv THEN BEGIN
        f = [f[0,*],f[2:*,*]]
        phi = replicate(1, (size(f, /dim))[0]) # ls_int(pha) * f
    ENDIF ELSE phi = ls_int(pha) * f

    ; albedo scaling
    IF keyword_set(a_ls) THEN BEGIN
        phi0 = phi[0,*]
        phi *= a_ls/2
        IF calc_deriv THEN  $
            phi = [phi[0,*], phi0, phi[1:*,*]]
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
