;+
; NAME:
;
;  mpfit_photo.pro
;
; PURPOSE:
;
;  Fit photometric parameters input for bidirectional reflectance
;  measurements
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  parms = mpfit_photo(start_parms, inc, emi, pha, iof[, model=model]
;          [err=array][, fixed=array][, limits=array][, serror=variable]
;          [, rms=variable][, /ver][, other mpfit_keywords...])
;
; INPUTS:
;
;  start_parms  - Starting parameters
;  inc   - Incidence angle
;  emi   - Emission angle
;  pha   - Phase angle
;  iof   - Measured I/F
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;  err    - Measurement errors for input i/f.
;  model  - A number to specify the model to be used.  The keys are listed:
;              1   - Hapke model, with isotropic multiple scattering, 5
;                    parameters: SSA, B0, h, g, theta.  This is the default
;              2   - Hapke model, with anisotropic multiple scattering,
;                    5 parameters: SSA, B0, h, g, theta.
;              3   - Hapke model, with anisotropic multiple scattering and
;                    porosity included.  6 parameters: SSA, B0, h, g, theta
;                    and phi, where phi is filling factor.
;              4   - Hapke model, with anisotropic multiple scattering,
;                    7 parameters: SSA, B0, h, g, theta, Bs0, hs, i.e.,
;                    including CBOE on top of model No. 2
;              5   - Same as model 4, but include filling factor.  8
;                    parameters SSA, B0, h, g, theta, Bs0, hs, and phi.
;              6   - Hapke model, with isotropic multiple scattering,
;                    and use double-term HG function.  It includes 6
;                    parameters: SSA, B0, h, theta, g, and c.  Here the
;                    two asymmetry factors for double-term HG function
;                    are assumed to be the same, g, and the partiction
;                    parameter is c.  It does not include CBOE.  This is the
;                    version adopted by USGS photometric correction routine
;                    "photomet".
;              7   - The same as 6, but uses anisotropic multiple scattering.
;              8   - Same as model 7, but include CBOE.  It includes 8
;                    parameters: SSA, B0, h, theta, g, c, Bs0, and hs.
;              9   - Same as model 7, but include filling factor.  It
;                    includes 7 parameters: SSA, B0, h, theta, g, c, and
;                    phi.
;              10  - Same as model 8, but include filling factor.  9
;                    parameters: SSA, B0, h, theta, g, c, Bs0, hs, phi.
;              11  - This version includes everything, and assumes that
;                    the two parameters in double-term HG function are
;                    different, and it uses anisotropic multiple scattering.
;                    10 parameters: SSA, B0, h, theta, g1, g2, c, Bs0, hs,
;                    and phi.
;  fixed  - An array of the same length of parameters.  If set non-zero,
;           the corresponding parameter is fixed and not fitted.
;  rms    - A variable returns the model rms
;  limits - A 2-D array of (NP, 2) sets the lower and upper limit of NP
;           to be parameters.
;  serror - A variable returns the scaled errors to fitted parameters.
;           serror = perror * sqrt(bestnorm/dof), where
;           dof = number of iof - number of parameters
;  verbose- If set, program prints out information and results on screen.
; 
;  Program also accepts all keyword parameters that mpfit.pro accepts
;
; OUTPUTS:
;
;  Function returns the best-fit parameters.
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
;  Program calls mpfit_photofunc.pro as the user supplied function to
;  evaluate the model I/F based on the input model parameters and
;  geometric parameters.
;
; PROCEDURE:
;
;  This program is an implimentation of the MPFIT.pro (Markwardt, 2008)
;  in fitting photometric models to I/F measurements.  It takes the I/F
;  measurements and an initial guess of a set of photometric parameters,
;  as input and finds the best-fit parameters for the specified photometric
;  models.
;
;  Markwardt, C. B., 2008, Non-Linear Least Squares Fitting in IDL with
;  MPFIT.  In Bohlender, D., Dowler, P., Durand, D., (Eds.), Proceeding
;  Astronomical Data Analysis Software and Systems XVIII,Quebec, Canada,
;  ASP Conference Series 411 (Astronomical Society of the Pacific: San
;  Francisco),p. 251-254.
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  12/3/2012, created by JYL @PSI
;-

function mpfit_photo, p0, inc, emi, pha, iof, error=err, model=model, aims=aims, fixed=fixed, rms=rms, chisq=chisq, limits=limits, serror=serror, bestnorm=bestnorm, perror=perror, status=status, errmsg=errmsg, verbose=ver, _ref_extra=_ref_extra

  if n_params() ne 5 then begin
    message,/con, 'Usage: parms = mpfit_photo(start_parms, inc, emi, pha, iof[, model=model][err=array][, fixed=array][, limits=array][, serror=variable][, rms=variable][, /ver][, other mpfit_keywords...])'
    print
    print, '     model  - A number to specify the model to be used.  For each'
    print, '              case, the parameters has to be in the order as listed'
    print, '              below.  The keys are listed:'
    print, '        1   - [SSA, B0, h, g, theta]'
    print, '              5 parameters version.  This is the default'
    print, '        2   - [SSA, B0, h, g, theta, phi]'
    print, '              Model 1 plus porosity.  6 parameters, where phi is'
    print, '              the filling factor.'
    print, '        3   - [SSA, B0, h, g, theta, Bs0, hs]'
    print, '              Model 1 plus CBOE.  7 parameters, where Bs0 and h'
    print, '              are the amplitude and width parameters for CBOE.'
    print, '        4   - [SSA, B0, h, g, theta, Bs0, hs, phi]'
    print, '              Model 3 plus filling factor.  8 parameters.'
    print, '        5   - [SSA, B0, h, g, theta, c]'
    print, '              Similar to model 1 but uses double-term HG function.'
    print, '              It includes 6 parameters.  The two asymmetry factors'
    print, '              for double-term HG function are assumed to be the same'
    print, '              as g, and the partiction parameter is c.  This is the'
    print, '              version adopted by USGS photometric correction routine'
    print, '              "photomet".'
    print, '        6   - [SSA, B0, h, g, theta, c, Bs0, hs]'
    print, '              Similar to model 5, but includes CBOE.  8 parameters.'
    print, '        7   - [SSA, B0, h, g, theta, c, phi]'
    print, '              Similar to model 5, but includes filling factor.  It'
    print, '              contains 7 parameters.'
    print, '        8   - [SSA, B0, h, g, theta, c, Bs0, hs, phi]'
    print, '              Similar to 6, but includes both CBOE and filling factor.'
    print, '              It contains 9 parameters.'
    print, '        9   - [SSA, B0, h, g1, theta, g2, c, Bs0, hs, phi]'
    print, '              This version includes everything, and assumes that'
    print, '              the two parameters in double-term HG function are'
    print, '              different.'
    return, 0.
  endif


  ; process input keywords
  if size(model,/type) eq 0 then model=1
  if model lt 1 and model gt 9 then begin
    print, 'Specified model does not found.  Use default model=1, 5-parameter version.'
    model=1
  endif

  nparms = n_elements(p0)
  ; check input parameters
  parm_err = 0
  case model of
    1: if nparms ne 5 then parm_err=1
    2: if nparms ne 6 then parm_err=1
    3: if nparms ne 7 then parm_err=1
    4: if nparms ne 8 then parm_err=1
    5: if nparms ne 6 then parm_err=1
    6: if nparms ne 8 then parm_err=1
    7: if nparms ne 7 then parm_err=1
    8: if nparms ne 9 then parm_err=1
    9: if nparms ne 10 then parm_err=1
  endcase
  if parm_err then begin
    message,/con,'Error, wrong number of initial parameters!  No iteration performed!'
    return, p0
  endif

  npts = n_elements(iof)
  if size(fixed,/type) eq 0 then fixed=replicate(0,nparms)
  if size(limits,/type) eq 0 then limits=replicate(1.,nparms)#[0d,0]
  if size(err,/type) eq 0 then err=replicate(1.,npts)
  if size(ver,/type) eq 0 then ver=0
  if size(aims,/type) eq 0 then aims=0

  ; prepare input to mpfit
  args = {inc:inc, emi:emi, pha:pha, iof:iof, err:err, model:model, aims:aims}

  parinfo = replicate({value:0.d, fixed:0, limited:[0,0], limits:[0.d,0], parname:''},nparms)
  parinfo[0].parname = '  SSA'
  parinfo[1].parname = '   B0'
  parinfo[2].parname = '    h'
  parinfo[3].parname = '    g'
  parinfo[4].parname = 'theta'
  parinfo[*].value = p0
  parinfo[*].fixed = fixed

  for i=0, nparms-1 do begin
    if limits[i,0] lt limits[i,1] then begin
      parinfo[i].limited = 1
      parinfo[i].limits = limits[i,*]
    endif else parinfo[i].limited=0
  endfor

  ; configure parameters for the model to be used
  case model of
    1:
    2: parinfo[5].parname = '  phi'
    3: begin
         parinfo[5].parname = '  Bs0'
         parinfo[5].parname = '   hs'
       end
    4: begin
         parinfo[5].parname = '  Bs0'
         parinfo[6].parname = '   hs'
         parinfo[7].parname = '  phi'
       end
    5: parinfo[5].parname = '    c'
    6: begin
         parinfo[5].parname = '    c'
         parinfo[6].parname = '  Bs0'
         parinfo[7].parname = '   hs'
       end
    7: begin
         parinfo[5].parname = '    c'
         parinfo[6].parname = '  phi'
       end
    8: begin
         parinfo[5].parname = '    c'
         parinfo[6].parname = '  Bs0'
         parinfo[7].parname = '   hs'
         parinfo[8].parname = '  phi'
       end
    9: begin
         parinfo[3].parname = '   g1'
         parinfo[5].parname = '   g2'
         parinfo[6].parname = '    c'
         parinfo[7].parname = '  Bs0'
         parinfo[8].parname = '   hs'
         parinfo[9].parname = '  phi'
       end
  endcase

  ; model fitting
  fitp = mpfit('mpfit_photofunc', p0, functargs=args, parinfo=parinfo, quiet=~ver, status=status, errmsg=errmsg, _extra=_ref_extra,perror=perror,bestnorm=bestnorm)
  chisq = bestnorm
;  dev = mpfit_photofunc(fitp,inc=inc,emi=emi,pha=pha,iof=iof,err=err,model=model)
;  chisq = total(dev*dev)

  if status ge 1 and status le 9 then begin
    ; post-fit processing and scaling
    dof = n_elements(inc)-nparms
    dev = mpfit_photofunc(fitp,inc=inc,emi=emi,pha=pha,iof=iof,model=model)
    rms = sqrt(avg(dev*dev))/avg(iof)
    serror = perror*sqrt(chisq/dof)
  endif

  ; print out results
  if ver then begin
    print
    if status ge 1 and status le 3 then print, 'Fit successful!'
    if status ge 4 and status le 8 then print, 'Fit complete, but with warnings.'

    case status of
      -18: if errmsg ne '' then print, 'Fatal error: '+errmsg  $
               else print, 'Fatal error!'
      -16: print, 'Error: a parameter or function value has become infinite or an undefined number!'
      0: print, 'Error: Improper input parameters!'
      1: print, 'Both the actual and predited relative reductions in the sum of squares are at most FTOL.'
      2: print, 'Relative error between two consecutive iterates is at most XTOL.'
      3: print, 'Both the actual and predited relative reductions in the sum of squares are at most FTOL.  Relative error between two consecutive iterates is at most XTOL.'
      4: print, 'The cosine of the angle between fvec and any column of the jacobian is at most GTOL in absolute value.'
      5: print, 'The maximum number of iterations has been reached.'
      6: print, 'FTOL is too small.  No further reduction in the sum of squares is possible.'
      7: print, 'XTOL is too small.  No further improvement in the approximate solution is possible.'
      8: print, 'GTOL is too small.  fvec is orthogonal to the columns of the jacobian to machine precission.'
      9: print, 'A successful single iteration has been completed, and the user must supply another "EXTERNAL" evaluation of the function and its derivatives.'
      else: print, 'Unknow error.'
    endcase

    if status ge 1 and status le 9 then begin
      numstr = strtrim(string(nparms,format='(i2)'),2)
      print
      print, fitp,    format='("Best-fit results  :", '+numstr+'(f7.4,"  "))'
      print, serror,  format='("Scaled uncertainty:", '+numstr+'(f7.4,"  "))'
      print, perror,  format='("Raw uncertainty   :", '+numstr+'(f7.4,"  "))'
      print, rms*100, format='("RMS: ", f6.2, "%")'
      print, geoalbedo(fitp[0],fitp[1],fitp[3],fitp[4]),format='("Geometric albedo: ", f7.3)'
      print, bondalbedo(fitp[0],fitp[1],fitp[2],fitp[3],fitp[4]),format='("Bond albedo: ", f7.3)'
    endif
  endif

  return, fitp

end
