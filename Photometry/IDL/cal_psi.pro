; Geometric relation of i, e, alpha, and psi.  Given i, e, and alpha,
; calculate psi.  alpha must satisfy abs(i-e) <= alpha <= i+e
; If set keyword 'cos', then the input i and e are actually cos(i)
; and cos(e), or mu0 and mu.  All angles are measured in deg.
;
; Note:  Because of computational round-off error, sometimes this routine
; can't give out a correct answer for psi even all input parameters are
; valid.  This happens when psi is close to 0 or 180 degree.
;
; 5/3/2004, updated by JL, eliminate a bug happened when input i, e, and
;    alpha have different elements.
; 12/2/2011, updated by JYL @UMd,
;   * make the calculation when psi=0 and 180 deg more robust


function cal_psi, i, e, alpha, cos=cos

  nalpha = n_elements(alpha)
  n_i = n_elements(i)
  n_e = n_elements(e)

  if keyword_set(cos) then begin   ; inputs are mu_0 and mu
    mu0 = i  &  mu = e
    si = sqrt(1-mu0^2)  &  se = sqrt(1-mu^2)
  endif else begin  ; inputs are angles in degrees
    mu0 = cos(i*!dpi/180)  &  mu = cos(e*!dpi/180)
    si = sin(i*!dpi/180)  &  se = sin(e*!dpi/180)
  endelse

  nn = max([nalpha,n_i,n_e])
  alpha0 = alpha*!dpi/180
  if nn gt 1 then begin
    if nalpha eq 1 then alpha0 = replicate(alpha0,nn)
    if n_i eq 1 then si = replicate(si,nn)
    if n_e eq 1 then se = replicate(se,nn)
  endif

  psi = alpha0*0d
  w = where(si*se ne 0, ct)
  if ct gt 0 then begin
    psi[w] = (cos(alpha0[w])-mu0[w]*mu[w])/(si[w]*se[w])
    w0 = where(abs(1-abs(psi[w])) le 1e-7)
    w1 = where(1-abs(psi[w]) gt 1e-7)
    w2 = where(1-abs(psi[w]) lt -1e-7)
    if w0[0] ne -1 then psi[w[w0]] = (-sign(psi[w[w0]])>0)*180
    if w1[0] ne -1 then psi[w[w1]] = acos(psi[w[w1]])*180/!dpi
    if w2[0] ne -1 then psi[w[w2]] = !values.d_nan
  endif

  return, psi

end
