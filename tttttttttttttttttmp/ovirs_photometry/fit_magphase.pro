;+
; NAME:
;
;  fit_magphase.pro
;
; PURPOSE:
;
;  Fit disk-integrated phase function in magnitude
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  par = fit_magphase(model, mag, phase, radius, magerr=magerr, msun=-26.74)
;
; INPUTS:
;
;  model   - A string to specify what model to fit.  The options are
;            'lommel-seeliger', 'rolo', 'mcewen' (case insensitive).  If
;            the specified model is not recognized, then program will
;            generate an error.
;  mag     - Magnitudes of target
;  phase   - Phase angles
;  radius  - Radius of target in km
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  magerr  - Magnitude error
;  msun - Solar magnitude corresponding to the band of `ref`, default is
;         V-band -26.74
;
; OUTPUTS:
;
;  Returns the best-fit parameters
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
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;-

FUNCTION int_model, p, model=model, mag=mag, pha=pha, err=err, radius=radius, msun=msun

    ref = phase_func(model, p, pha)
    vmod = ref2mag(ref, radius, msun=msun)
    if KEYWORD_SET(err) THEN RETURN, (vmod - mag) / err  $
    ELSE RETURN, vmod - mag

END


FUNCTION fit_magphase, model, mag, phase, radius, msun=msun, p0=p0, magerr=magerr, fixed=fixed, chisq=chisq, quiet=quiet, status=s

    IF not keyword_set(quiet) THEN quiet=!quiet

    ; check model selection and initial parameters
    IF strcmp(model, 'lommel-seeliger', 5, /fold_case) EQ 1 THEN  $
        p00 = [0.1, -0.0368, 1e-5, 1e-9]  $
    ELSE IF strcmp(model, 'rolo', 5, /fold_case) EQ 1 THEN  $
        p00 = [0., 0.2, 0.3, 7.0e-4, -1e-4, 1.0e-8, 6.0e-7]  $
    ELSE IF strcmp(model, 'mcewen', 5, /fold_case) EQ 1 THEN  $
        p00 = [0.1, -0.0368, 1e-5, 1e-9, -4e-2, 5e-9, 3e-6]  $
    ELSE IF strcmp(model, 'minnaert', 5, /fold_case) EQ 1 THEN  $
        p00 = [0.1, 0.035, 3.5e-7, -3.5e-9, 0.5, 0.0035]  $
    ELSE IF strcmp(model, 'akimov', 5, /fold_case) EQ 1 THEN  $
        p00 = [0.1, -0.033, 2e-5, -4.5e-7]  $
    ELSE message, 'model not recognized or not implemented: '+model

    IF NOT keyword_set(p0) THEN p0 = p00
    np = n_elements(p0)
    npcorr = n_elements(p00)
    IF np NE npcorr THEN $
        message, 'number of initial parameters ('+string(np)+') not consistent with specified model ('+model+'), '+string(npcorr)+' parameters expected'

    parinfo = replicate({value: 0., limited: [0, 0], limits: [0, 0], fixed: 0}, npcorr)
    FOR i=0, npcorr-1 DO parinfo[i].value=p0[i]
    parinfo[0].limited = [1, 0]
    parinfo[0].limits[0] = 0.
    IF keyword_set(fixed) THEN FOR i=0, n_elements(fixed)-1 DO parinfo[i].fixed=fixed[i]

    ; fit model with mpfit
    IF keyword_set(magerr) THEN fa = {model: model, pha: phase, mag: mag, err: err, radius: radius}  $
    ELSE fa = {model: model, pha: phase, mag: mag, radius: radius}
    IF keyword_set(msun) THEN fa = create_struct(fa, 'msun', msun)
    p = mpfit('int_model', parinfo=parinfo, perror=perror, functargs=fa, quiet=quiet, COVAR=covar, status=s, bestnorm=chisq)

    p_covar=[[p],[covar]]
    return, p_covar

END
