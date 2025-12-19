;+
; NAME:
;
;  phase_func.pro
;
; PURPOSE:
;
;  Calculate disk-integrated phase function
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  phi = phase_func(model, p, pha[, /normalized])
;
; INPUTS:
;
;  model      - A string to specify the photometric model name
;  p          - An array of photometric model parameters
;  pha        - Phase angle in degrees
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  normalized - If set, then the phase function is normalized to unity at zero
;               phase angle (phi(0) = 1).  Default is to return the phase
;               function such at phi(0) = geometric albedo, i.e., not
;               normalized
;  _ref_extra - Extra input and output keywords as allowed by individual
;               integrated phase function model
;
; OUTPUTS:
;
;  Returns the phase function.
;
;  If `deriv' is set, then program returns the model reflectance and
;  derivatives w/r to the model parameters in a 2-D array of dimensions
;  (N+1)xM, where N is the number of parameters, so [0,*] elements in the
;  return is the phase function and [1:*,*] is the derivatives.
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
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : July 14, 2016, created by JYL @PSI
;  v1.0.1 : February 2, 2017, modified by JYL @PSI
;    Fixed a bug in the calculation for Minnaert model
;  v2.0 : March 15, 2017, JYL
;    Major reconstruction to modulize flow and fix bugs
;    NOTE: Formula for Minnaert model is still wrong, need to correct
;-


PRO model_par_error, model, np0, np1
    on_error, 2
    message, 'number of model parameters ('+string(np1)+') not consistent with the specified model ('+model+'), '+string(np0)+' parameters expected'
END

FUNCTION phase_func, model, p, pha, normalized=normalized, _ref_extra=ex

    np = n_elements(p)
    IF NOT keyword_set(normalized) THEN a_norm = p[0]
    IF strcmp(model, 'lommel-seeliger', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 4 THEN model_par_error, model, 4, np
        phi = ls_intphasefunc(pha, p[1], p[2], p[3], a_ls=a_norm, $
                    _extra=ex)
    ENDIF ELSE IF strcmp(model, 'rolo', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 7 THEN model_par_error, model, 7, np
        phi = rolo_intphasefunc(pha, p[0], p[1], p[2], p[3], p[4], p[5],  $
                    p[6], normalized=normalized, _extra=ex)
    ENDIF ELSE IF strcmp(model, 'minnaert', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 6 THEN model_par_error, model, 6, np
        phi = minnaert_intphasefunc(pha, p[1], p[2], p[3], p[4], p[5],  $
                    a_m=a_norm, _extra=ex)
    ENDIF ELSE IF strcmp(model, 'mcewen', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 7 THEN model_par_error, model, 7, np
        phi = mcewen_intphasefunc(pha, p[1], p[2], p[3], p[4], p[5], p[6],  $
                    a_mc=a_norm, _extra=ex)
    ENDIF ELSE IF strcmp(model, 'akimov', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 4 THEN model_par_error, model, 4, np
        phi = akimov_intphasefunc(pha, p[1], p[2], p[3], a_ak=a_norm,  $
                    _extra=ex)
    ENDIF ELSE message, 'model not implemented: '+model

    RETURN, phi

END
