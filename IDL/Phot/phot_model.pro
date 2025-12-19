;+
; NAME:
;
;  phot_model.pro
;
; PURPOSE:
;
;  Wrapper function for all photometric models
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  ref = phot_model(model, pha, inc, emi, p[, _extra=_extra])
;
; INPUTS:
;
;  model      - A string to specify the model.  The options are
;               'lommel-seeliger', 'rolo', 'minnaert', mcewen', 'akimov' (case
;               insensitive).  Note that only the first five characters in
;               the model name are checked.  If the specified model is not
;               recognized, then program will generate an error.
;  pha, inc, emi - Phase angle, incidence angle, and emission angle in degrees
;  p          - Array containing the photometric model parameters.  The order
;               of parameters in the array follows the order in the API of the
;               corresponding photometric model routines, as listed below:
;                   Lommel-Seeliger: a_ls, p1, p2, p3
;                   ROLO: c0, c1, a0, a1, a2, a3, a4
;                   Minnaert: a_m, p1, p2, p3, k0, b
;                   McEwen: a_mc, p1, p2, p3
;                   Akimov: a_ak, m, u1, u2
;               If the number of parameters provided is not sufficient for the
;               specified model, an error will be generated.
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  _extra     - All keyword parameters accepted by the functions to calculate
;  each individual model.
;
; OUTPUTS:
;
;  Returns photometric models.  See lommel_seeliger_model.pro, rolo_model.pro
;  minnaert_model.pro, mcewen_model.pro, and akimov_model.pro for details
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
;
; PROCEDURE:
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : May 21, 2016, created by JYL @PSI
;  v1.0.1 : March 15, 2017, JYL
;    Return to caller on error for model_par_error
;-


PRO model_par_error, model, np0, np1
    on_error, 2
    message, 'number of model parameters ('+string(np1)+') not consistent with the specified model ('+model+'), '+string(np0)+' parameters expected'
END


FUNCTION phot_model, model, pha, inc, emi, p, _extra=_extra

    np = n_elements(p)
    IF strcmp(model, 'lommel-seeliger', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 4 THEN model_par_error, model, 4, np
        RETURN, lommel_seeliger_model(pha, inc, emi, p[0], p[1], p[2], p[3], _extra=_extra)
    ENDIF ELSE IF strcmp(model, 'rolo', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 7 THEN model_par_error, model, 7, np
        RETURN, rolo_model(pha, inc, emi, p[0], p[1], p[2], p[3], p[4], p[5], p[6], _extra=_extra, radf=radf, reff=reff, brdf=brdf)
    ENDIF ELSE IF strcmp(model, 'minnaert', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 6 THEN model_par_error, model, 6, np
        RETURN, minnaert_model(pha, inc, emi, p[0], p[1], p[2], p[3], p[4], p[5], _extra=_extra)
    ENDIF ELSE IF strcmp(model, 'mcewen', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 7 THEN model_par_error, model, 7, np
        RETURN, mcewen_model(pha, inc, emi, p[0], p[1], p[2], p[3], p[4], p[5], p[6], _extra=_extra)
    ENDIF ELSE IF strcmp(model, 'akimov', 5, /fold_case) EQ 1 THEN BEGIN
        IF np LT 4 THEN model_par_error, model, 4, np
        RETURN, akimov_model(pha, inc, emi, p[0], p[1], p[2], p[3], _extra=_extra)
    ENDIF ELSE message, 'model not recognized: '+model

END
