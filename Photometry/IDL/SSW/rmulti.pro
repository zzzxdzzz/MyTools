
; Multiply scattered reflectance
; Hapke 1981, 2002
;
; 12/17/2012, modified by JYL @PSI
;   * changed the keyword "new", which is never used, to "aims" meaning
;     anisotropic multiple scattering.
;   * add keywords pfunc_form and pfunc_param to select different forms
;     of particle phase function.  It is a number from 1 to 3.  1: (default)
;     single-term HG; 2: double-term HG; 3: polynomial.


pro get_p, mu0, mu, pmu0, pmu, p, b, eps=eps, niter=niter, pfunc_form=form, pfunc_param=param
; compute the 3 P integrals P(mu0), P(mu), and P
; cf. Hapke 2002
; The iteration number for mu0=mu=1 is inversely proportional to
; 1-b.  The impirical approximation is:
; n = 33.6/(1-b)^1.03
; when 1-b is close to 1, the actually n is even smaller than
; computed from above formula
; For most asteroids, they are close to isotropic, so n is less
; than 100
;
; mu0, mu: the cosine of i and e
; pmu0, pmu, p: the three P integrals returned
; b: the parameter of single particle phase function
; eps: the relative accuracy parameter of computing pmu0, pmu, and p
;      default 1e-10
;
; 12/17/2012, modified by JYL @PSI
;   * add keywords pfunc_form, pfunc_param to accomodate other forms of single-particle
;   phase functions
;   * add keyeword niter to return the acutal number of iterations
;   * remove keyword nmax.  set the maximum iterations to be n=100.  program
;   will issue a warning if the maximum number of iterations is reached but
;   the designated accuracy is not reached.
;   * set the parameters An so program doesn't need to calculate them.
;
; 12/18/2012, modified by JYL @PSI
;   1. add keyword phi to include the porosity parameter as developed in
;      Hapke (2008).
;   2. remove the 4*pi/w*mu0/(mu0+mu) term, so that the rsingle part only
;      contains the multiple scattering term.
;
; 7/6/2013, modified by JYL @PSI
;   1. Corrected a bug that cause mistake when pass to rmulti without
;      g-parameter for isotropic case.



; parameter An
an = [-5.000000000000e-01,  1.250000000000e-01, -6.250000000000e-02,  3.906250000000e-02, -2.734375000000e-02,  2.050781250000e-02, -1.611328125000e-02,  1.309204101562e-02, -1.091003417969e-02,  9.273529052734e-03, -8.008956909180e-03,  7.007837295532e-03, -6.199240684509e-03,  5.535036325455e-03, -4.981532692909e-03,  4.514514002949e-03, -4.116174532101e-03,  3.773159987759e-03, -3.475278936094e-03,  3.214633015887e-03, -2.985016371895e-03,  2.781492528356e-03, -2.600090841725e-03,  2.437585164117e-03, -2.291330054270e-03,  2.159137935754e-03, -2.039185828212e-03,  1.929943730272e-03, -1.830119054569e-03,  1.738613101840e-03, -1.654486661429e-03,  1.576932599174e-03, -1.505253844666e-03,  1.438845586813e-03, -1.377180775950e-03,  1.319798243619e-03, -1.266292909418e-03,  1.216307662993e-03, -1.169526599032e-03,  1.125669351568e-03, -1.084486326511e-03,  1.045754671993e-03, -1.009274857854e-03,  9.748677604267e-04, -9.423721684125e-04,  9.116426411816e-04, -8.825476632716e-04,  8.549680487943e-04, -8.287955575047e-04,  8.039316907796e-04]
; parameter An^2
an2 = [2.500000000000e-01,  1.562500000000e-02,  3.906250000000e-03,  1.525878906250e-03,  7.476806640625e-04,  4.205703735352e-04,  2.596378326416e-04,  1.714015379548e-04,  1.190288458019e-04,  8.599834109191e-05,  6.414339077310e-05,  4.910978356065e-05,  3.843058506448e-05,  3.063662712410e-05,  2.481566797052e-05,  2.038083668282e-05,  1.694289277871e-05,  1.423673629322e-05,  1.207756368366e-05,  1.033386542683e-05,  8.910322740480e-06,  7.736700685303e-06,  6.760472385220e-06,  5.941821432322e-06,  5.250193417600e-06,  4.661876625613e-06,  4.158278841982e-06,  3.724682802017e-06,  3.349335753895e-06,  3.022775517890e-06,  2.737326112845e-06,  2.486716422338e-06,  2.265789136882e-06,  2.070276622692e-06,  1.896626889646e-06,  1.741867403859e-06,  1.603497732442e-06,  1.479404331057e-06,  1.367792465844e-06,  1.267131489061e-06,  1.176110592389e-06,  1.093602833995e-06,  1.018635738695e-06,  9.503671503194e-07,  8.880653037984e-07,  8.310923052206e-07,  7.788903779461e-07,  7.309703644592e-07,  6.869020761396e-07,  6.463061634397e-07]

  if not keyword_set(eps) then eps = 1e-10

  if size(form,/type) eq 0 then form=1

;                 1: (default) single-term HG
;                 2: double-term HG
;                 3: polynomial

  nn = findgen(100)+1
  case form of
    ; 1: (default) single-term HG
    1: bn = (2*nn+1)*(-b)^nn
    ; 2: double-term HG
    2: bn = param[2]*(2*nn+1)*param[0]^nn
  endcase

  pmu0 = mu0  &  pmu0[*] = 1d  &  pmu = pmu0  &  p = 1.

  n = 1  &  i = 0  ;   &  facfrac = 1d  &  bpower = 1d  &  sign = -1

  repeat begin
    imp = 0                   ; reset improvement flag
    legmu0 = legendre(mu0,n)
    legmu = legendre(mu,n)
    deltapmu0 = an[i]*bn[n-1]*legmu0
    deltapmu = an[i]*bn[n-1]*legmu
    deltap = an2[i]*bn[n-1]
    ww = where(abs(deltapmu0/pmu0) gt eps)
    if ww[0] ne -1 then begin
      pmu0[ww] += deltapmu0[ww]
      imp = 1
    endif
    ww = where(abs(deltapmu/pmu) gt eps)
    if ww[0] ne -1 then begin
      pmu[ww] += deltapmu[ww]
      imp = 1
    endif
    if abs(deltap/p) gt eps then begin
      p += deltap
      imp = 1
    endif
    n += 2
    i ++
  endrep until (not imp) or (n gt 100)


  if imp then print, 'Warning: maximum number of iteration reached.'
  return

end


function rmulti, w, g, i, e, alpha, cos=cos, aims=aims, pfunc_form=form, pfunc_param=pfunc_param, phi=phi
; if set keyword 'aims', then use new version of Hapke's multi-scattering
; model with anisotropic multiple scattering (AIMS), which is about
; 3.4 times slower than old version

  if n_params() lt 4 then begin
    print, 'Usage: rmulti, w, [g,] i, e, alpha[, /cos, /new]'
    return, 0.
  endif

  if n_params() eq 4 then begin
    alpha = e
    e = i
    i = g
  endif

  if not keyword_set(cos) then cos=0
  if not cos then begin
    mu0 = cos(i/180.*!dpi)
    mu = cos(e/180.*!dpi)
  endif else begin
    mu0 = i
    mu = e
  endelse

  if size(phi,/type) ne 0 then begin
    if phi lt 0 or phi gt 0.752 then begin
      print, 'Warning: invalid filling factor.  Reset to 0 (not included).'
      phi = 0
    endif
    if phi eq 0 then kk=1.
    if phi gt 0 and phi le 0.752 then begin
      phi23 = 1.209*phi^0.6666666666667d
      kk = -alog(1-phi23)/phi23
    endif
  endif

  if not keyword_set(aims) then begin
    ; isotropic multiple scattering function
    if size(kk,/type) ne 0 then return, hfunc(w,mu0/kk)*hfunc(w,mu/kk)-1  $
      else return, hfunc(w,mu0)*hfunc(w,mu)-1
  endif else begin
    ; new version of aisotropic multiple scattering function, c.f. Hapke 2002
    get_p, mu0, mu, pmu0, pmu, p, g    ; compute 3 P's
    if size(kk,/type) ne 0 then begin
      hmu0 = hfunc(w,mu0/kk)
      hmu = hfunc(w,mu/kk)
    endif else begin
      hmu0 = hfunc(w,mu0)
      hmu = hfunc(w,mu)
    endelse
    return, pmu0*(hmu-1)+pmu*(hmu0-1)+p*(hmu-1)*(hmu0-1)
  endelse

end
