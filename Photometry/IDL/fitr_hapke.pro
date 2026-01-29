;+
; NAME:
;
;  fitr_hapke.pro
;
; PURPOSE:
;
;  Fit Hapke parameters to I/F data.
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  parms = fitr_hapke(inc, emi, pha, iof[, err=err][, model=model][, limits=limits]
;              [, np0=np0][, maxiter=maxiter][, parms0=variable]
;              [, rms=variable][, chisq=variable][, perror=variable]
;              [, serror=variable][, status=variable][, tr_parms=variable]
;              [, tr_rms=variable][, tr_perror=variable][, tr_serror=variable]
;              [, tr_status=variable][, /verbose][, /plot])
;
; INPUTS:
;
;  inc     - Incidence angle in degrees
;  emi     - Emission angle in degrees
;  pha     - Phase angle in degrees
;  iof     - Measured I/F to be fitted
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;  err     - Measurement errors of I/F data
;  model   - Specify the version of Hapke model to be used.  Currently
;            only the 5-parameter version is implemented.
;  limits  - A (NPx2) array specify the lower and upper limits of each
;            parameter to be fitted.  If the lower limit equals upper
;            limit, then it implies that the corresponding parameter is
;            kept fixed in the fit.  The defaults are:
;                SSA: [0, 1]
;                B0: [0, 10]
;                h: [0, 1]
;                g: [-1, 1]
;                theta_bar: [0, 60]
;  np0     - Number of initial parameter sets to be tried.  Default is 100.
;  maxiter - Number of iterations by MPFIT.  Default is 200.
;  rms     - Returns the best-fit RMS: sqrt(avg((data-model)^2))/avg(iof)
;  chisq   - Returns the best-fit chi-square: total((data-model)^2)
;  perror  - Returns the errors as returned by MPFIT for the best-fit set
;            of parameters.
;  serror  - Returns the scaled errors for each best-fit parameter for
;            unweighted fit assuming the errors of input I/F should result
;            in unity for the best-fit chi-square.
;  status  - Returns the MPFIT status flag for the best-fit parameters
;  parms0  - Returns the initial parameter sets tried.
;  tr_parms - Returns the best-fit parameter sets for all the initial
;            parameters tried.
;  tr_rms  - Returns the RMS of all trials
;  tr_status - Returns the MPFIT status flags of all trials
;  tr_perror - Returns the MPFIT errors of parameters for all trials
;  tr_serror - Returns the scaled errors of parameters for all trials
;  verbose - If set, program prints out progress as it proceeds.  Default is
;            to supress all screen print.
;  plot    - Plot the graphic representation of results.  Default is to
;            supress plots.
;
; OUTPUTS:
;
;  Program returns the best-fit parameters
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
;  Program calls mpfit_photo.pro, which in turn calls mpfit_photofunc.pro.
;
; PROCEDURE:
;
;  This program takes input I/F measurements and finds the best-fit model
;  parameters for the specified version of Hapke model.
;
;  The basic fitting engine is MPFIT.pro (Markwardt, 2008).  However, this
;  program randomly generate a number of initial parameters within the
;  (default or specified) range of each parameter to start the MPFIT
;  iterations.
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

function fitr_hapke, inc, emi, pha, iof, err=err, model=model, aims=aims, limits=limits, np0=np0, maxiter=maxiter, parms0=parms0, rms=rms, chisq=chisq, sigma=sigma, perror=perror, serror=serror, status=status, tr_parms=parms, tr_rms=tr_rms, tr_perror=tr_perror, tr_serror=tr_serror, tr_status=tr_status, verbose=ver, plot=plot

  if n_params() ne 4 then begin
    message, /con, 'Usage: parms = fitr_hapke(inc, emi, pha, iof[, err=err][, model=model][, limits=limits][, np0=np0][, maxiter=maxiter][, parms0=variable][, rms=variable][, chisq=variable][, perror=variable][, serror=variable][, status=variable][, tr_parms=variable][, tr_rms=variable][, tr_perror=variable][, tr_serror=variable][, tr_status=variable][, /verbose][, /plot])'
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

  if size(np0,/type) eq 0 then np0=100
  if size(ver,/type) eq 0 then ver=0
  if size(plot,/type) eq 0 then plot=0

  if size(model,/type) eq 0 then model=1
  if model lt 1 and model gt 9 then begin
    print, 'Specified model does not found.  Use default model=1, 5-parameter version.'
    model=1
  endif

  ; populate initial parameters, based on the chosen model
  case model of
    1: nparms = 5
    2: nparms = 6
    3: nparms = 7
    4: nparms = 8
    5: nparms = 6
    6: nparms = 8
    7: nparms = 7
    8: nparms = 9
    9: nparms = 10
  endcase
        
  if size(limits,/type) eq 0 then begin
    limits = [[0., 0., 0., -1., 0.], [1., 10., 1., 1., 60.]]
    case model of
      1:
      2: limits = [limits,[[0.],[0.752]]]
      3: limits = [limits,[[0.,0.],[10,1]]]
      4: limits = [limits,[[0.,0,0],[10,1,0.752]]]
      5: limits = [limits,[[0.],[1]]]
      6: limits = [limits,[[0.,0.,0],[1.,10,1]]]
      7: limits = [limits,[[0.,0],[1,0.752]]]
      8: limits = [limits,[[0.,0,0,0],[1.,10,1,0.752]]]
      9: limits = [limits,[[-1.,0,0,0,0],[1.,1,10,1,0.752]]]
    endcase
  endif
  parms = (parms0 = fltarr(nparms,np0))
  tr_serror = (tr_perror = fltarr(nparms,np0))
  tr_rms = fltarr(np0)
  tr_status = intarr(np0)
  tr_chisq = fltarr(np0)
  r = reform(randomu(systime(1),np0*nparms),nparms,np0)
  for i=0,np0-1 do for j=0,nparms-1 do parms0[j,i]=r[j,i]*(limits[j,1]-limits[j,0])+limits[j,0]
  fixed = limits[*,0] eq limits[*,1]
  ww = where(fixed eq 0,ct)

  nfixed = n_elements(fixed)
  if nfixed ne nparms then begin
    print, 'Error: parameter limits wrong.  No iteration performed!'
    return, 0.
  endif

  ; try mpfit data with all initial parameter sets
  for i=0,np0-1 do begin
    if ver then begin
      print, 'Initial condition ',i+1
      print, parms0[*,i],format='('+strtrim(string(nparms,format='(i2)'),2)+'(f7.4,"  "))'
    endif

    parms[*,i] = mpfit_photo(parms0[*,i],inc,emi,pha,iof,error=err,fixed=fixed, limits=limits, serror=s, rms=r,chisq=c,ver=ver,perror=p,status=st,maxiter=maxiter,best_resid=bs,aims=aims,model=model)
    tr_serror[*,i] = s
    tr_perror[*,i] = p
    tr_rms[i] = r
    tr_status[i] = st
    tr_chisq[i] = c
  endfor

  ; find the global best fit
  ww = (where(tr_rms eq min(tr_rms)))[0]
  fitp = parms[*,ww]
  status = tr_status[ww]
  rms = tr_rms[ww]
  perror = tr_perror[*,ww]
  serror = tr_serror[*,ww]
  chisq = tr_chisq[ww]

;  ; estimate uncertainties
;  if ver then print, 'Estimating model parameter uncertainties'
;  errenv = 2   ; error envolope in terms of minimum chi-square
;  testparms = fitp
;  sigma = fltarr(5)
;  for i=0,4 do if not fixed[i] then begin
;    trial = (findgen(21)/10-1)*0.4*fitp[i]+fitp[i]
;    chisqtest = fltarr(21)
;    tr_fixed = fixed
;    tr_fixed[i] = 1
;    repeat begin
;      trial = (trial-fitp[i])*1.2+fitp[i]
;      testparms[i] = trial[0]
;      dev = mpfit_photofunc(testparms, inc=inc, emi=emi, pha=pha, iof=iof, err=err, model=model)
;      chisqtest[0] = total(dev*dev)
;      testparms[i] = trial[20]
;      dev = mpfit_photofunc(testparms, inc=inc, emi=emi, pha=pha, iof=iof, err=err, model=model)
;      chisqtest[20] = total(dev*dev)
;stop
;    endrep until (chisqtest[0] ge 1.5*errenv*chisq) and (chisqtest[20] ge 1.5*errenv*chisq)
;    for j=0,20 do begin
;      testparms[i] = trial[j]
;      tr_parms = mpfit_photo(testparms,inc,emi,pha,iof,error=err,fixed=tr_fixed,limits=limits,chisq=c,status=st,ver=ver,maxiter=maxiter)
;      chisqtest[j] = c
;    endfor
;    chisqdiff = abs(chisqtest-errenv*chisq)
;    ww = where(chisqdiff[0:10] eq min(chisqdiff[0:10]))
;    ww = ww[n_elements(ww)-1]
;    low = trial[ww]
;    ww = where(chisqdiff[10:20] eq min(chisqdiff[10:20]))
;    ww = ww[0]
;    high = trial[ww]
;    sigma[i] = (high-low)/2
;stop
;  endif
;stop

  ; print out best-fit results
  if ver then begin
    print
    print, 'Number of initial conditions tried: ', np0, format='(a,i5)'
    dummy = where(tr_status ge 1 and tr_status le 3, ct_suc)
    print, 'Number of successful fit: ', ct_suc, format='(a,i5)'
    if ct_suc ge 1 then begin
      numstr = strtrim(string(nparms,format='(i2)'),2)
      print, fitp,    format='("Best-fit results  :", '+numstr+'(f7.4, "  "))'
      print, serror,  format='("Scaled uncertainty:", '+numstr+'(f7.4, "  "))'
      print, perror,  format='("Raw uncertainty   :", '+numstr+'(f7.4, "  "))'
      print, rms*100, format='("RMS: ", f6.2, "%")'
      print, geoalbedo(fitp[0],fitp[1],fitp[3],fitp[4]),format='("Geometric albedo: ", f7.3)'
      print, bondalbedo(fitp[0],fitp[1],fitp[2],fitp[3],fitp[4]),format='("Bond albedo: ", f7.3)'
    endif
  endif

  ; graphic representation of the statistics of model fit
  if plot then begin
    tmp = where(~fixed,nf)
    !p.multi=[0,1,nf]
    for i=0,nparms-1 do if ~fixed[i] then begin  & $
      plothist,parms[i,*],bin=(limits[i,1]-limits[i,0])/20,xran=limits[i,*],charsize=2  & $
      plothist,parms0[i,*],bin=(limits[i,1]-limits[i,0])/20,/overplot,line=2  & $
      ver, fitp[i],line=1  & $
    endif
    !p.multi=0
  endif

  return, fitp

end
