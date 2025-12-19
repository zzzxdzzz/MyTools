;+
; NAME:
;
;  mpfit_phomodel.pro
;
; PURPOSE:
;
;  Fit a photometric model to data with MPFIT and return the best-fit
;  parameters and covariance matrix
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  pars = mpfit_phomodel(model, iof, pha, inc, emi[, ioferr=ioferr][, p0=p0][, /quiet
;
; INPUTS:
;
;  model      - A string to specify what model to fit.  The options are
;               'lommel-seeliger', 'rolo', 'minnaert', mcewen' (case
;               insensitive).  If the specified model is not recognized, then
;               program will generate an error.
;  iof        - The RADF (I/F) data to be fitted
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  pha        - Phase angle in degrees
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  ioferr     - The errors for `iof'
;  p0         - Initial parameters.  The defaults are:
;                  Lommel-Seeliger model: [0.1, -3e-3, 2e-9, 7e-7]
;                  ROLO model:[0., 0.2, 0.3, 7.0e-4, -1e-4, 1.0e-8, 6.0e-7]
;                  Minnaert model: [.1, 0.0, 0., 0., 0.5, 0.05]
;                  McEwen model: [0.1, -3e-3, 2e-8, 7e-7]
;               If number of parameters in `p0' is not compatible with the
;               requirement of the model specified in `model', then an error
;               will be generated
;
; OUTPUTS:
;
;  Program returns the best-fit parameters together with the covariance matrix
;  in a 2D array of Nx(N+1) in dimension
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
;   mpfit.pro (https://www.physics.wisc.edu/~craigm/idl/fitting.html)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : May 20, 2016, created by JYL @PSI
;  March 27, 2017, JYL @PSI
;    Change keyword `verbose` to `quiet`
;  Jun 28,2017, XDZ @PSI
;    Add perror request from mpfit.pro, return p and perror.
;-


FUNCTION target_phofunc, p, model=model, pha=pha, inc=inc, emi=emi, iofdata=iof, ioferr=ioferr

    vmod = phot_model(model, pha, inc, emi, p, /radf)
    if KEYWORD_SET(ioferr) THEN RETURN, (vmod - iof) / ioferr  $
    ELSE RETURN, vmod - iof

END


FUNCTION mpfit_phomodel, model, iof, pha, inc, emi, p0=p0, ioferr=ioferr, fixed=fixed,  quiet=quiet, status=s

    IF not keyword_set(quiet) THEN quiet=!quiet

    ; check model selection and initial parameters
    IF strcmp(model, 'lommel-seeliger', 5, /fold_case) EQ 1 THEN BEGIN
        p00 = [0.1, -0.0368, 1e-5, 1e-9]
    ENDIF ELSE IF strcmp(model, 'rolo', 5, /fold_case) EQ 1 THEN BEGIN
        p00 = [0.1, 0.0368, 0.3, 0,0,0,0]
    ENDIF ELSE IF strcmp(model, 'minnaert', 5, /fold_case) EQ 1 THEN BEGIN
        p00 = [.1, 0.04, 0., 0., 0.5, 0.04]
    ENDIF ELSE IF strcmp(model, 'mcewen', 5, /fold_case) EQ 1 THEN BEGIN
        p00 = [0.1, -0.0368, 1e-5, 1e-9, -4e-2, 5e-9, 3e-6]
    ENDIF ELSE IF strcmp(model, 'akimov', 5, /fold_case) EQ 1 THEN BEGIN
        p00 = [0.1, -0.0368, 1e-5, 1e-9]
    ENDIF ELSE BEGIN
        message, 'model not recognized: '+model
    ENDELSE

    IF NOT keyword_set(p0) THEN p0 = p00
    np = n_elements(p0)
    npcorr = n_elements(p00)
    IF np NE npcorr THEN $
        message, 'number of initial parameters ('+string(np)+') not consistent with specified model ('+model+'), '+string(npcorr)+' parameters expected'

    parinfo = replicate({value: 0., limited: [0, 0], limits: [0, 0], fixed: 0}, npcorr)
    FOR i=0, npcorr-1 DO parinfo[i].value=p0[i]
    parinfo[0].limited = [1, 0]
    parinfo[0].limits[0] = 0.
    IF keyword_set(fixed) THEN FOR i=0,n_elements(fixed) DO parinfo[i].fixed=fixed[i]

    ; fit model with mpfit
;    IF keyword_set(ioferr) THEN fa = {model: model, pha: pha, inc: inc, emi: emi, iofdata: iof, ioferr: ioferr}  $
;    ELSE fa = {model: model, pha: pha, inc: inc, emi: emi, iofdata: iof}
    fa = {model: model, pha: pha, inc: inc, emi: emi, iofdata: iof}
    p = mpfit('target_phofunc', parinfo=parinfo, perror=perror, functargs=fa, quiet=quiet, COVAR=covar, status=s)

    IF s LT 0 THEN BEGIN
        p = fltarr(np)
        covar = fltarr(np,np)
    ENDIF

    p_covar=[[p],[covar]]
    return, p_covar

END
