;+
; NAME:
;
;  mpfit_photofunc.pro
;
; PURPOSE:
;
;  User supplied function for fitting photometric functions for
;  MPFIT.  To be called by mpfit_photo.pro.
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  deviation = mpfit_photofunc(p, inc=inc, emi=emi, pha=pha, iof=iof,
;  err=err, model=model)
;
; INPUTS:
;
;  p   - Photometric parameters
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;  inc - Incidence angle in degrees
;  emi - Emission angle in degrees
;  pha - Phase angle in degrees
;  iof - Measured I/F values
;  err - Measurement errors associated with iof
;  model - A string to specify the photometric models used
;
; OUTPUTS:
;
;  Function returns the deviation between model and measurements,
;  defined as (measurements-model)/err.
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
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  12/3/2012, created by JYL @PSI
;  12/17/2012, modified by JYL @PSI
;    add keyword /aims to choose to use anisotropic multiple scattering form as
;    developed in Hapke (2002)
;-

function mpfit_photofunc, p, inc=inc, emi=emi, pha=pha, iof=iof, err=err, model=model, aims=aims

  nvar = n_elements(iof)
  if size(err,/type) eq 0 then err=replicate(1.,nvar)
  if size(aims,/type) eq 0 then aims=0
  if size(model,/type) eq 0 then model=1
  if model lt 1 or model gt 9 then begin
    print, 'Warning: specified model not found.  Use default (model=1), 5-parameter version.'
    model = 1
  endif

  ; choose model
  case model of
    1: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,aims=aims)
    2: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,phi=p[5],aims=aims)
    3: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,bs0=p[5],hs=p[6],aims=aims)
    4: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,bs0=p[5],hs=p[6],phi=p[7],aims=aims)
    5: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,pfunc_form=2,pfunc_param=[p[3],p[3],p[5]],aims=aims)
    6: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,pfunc_form=2,pfunc_param=[p[3],p[3],p[5]],bs0=p[6],hs=p[7],aims=aims)
    7: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,pfunc_form=2,pfunc_param=[p[3],p[3],p[5]],phi=p[6],aims=aims)
    8: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,pfunc_form=2,pfunc_param=[p[3],p[3],p[5]],bs0=p[6],hs=p[7],phi=p[8],aims=aims)
    9: rmod = rfunc(p[0],p[3],p[1],p[2],p[4],inc,emi,pha,pfunc_form=2,pfunc_param=[p[3],p[5],p[6]],bs0=p[7],hs=p[8],phi=p[9],aims=aims)
  endcase

  return, (iof-rmod*!pi)/err

end
