; Bidirectional reflectance
; Ref. Hapke 1981, 1984
;
; 10/13/2004, rename to rfunc_hapke.pro.  For the purpose of backward
; compatability, another dummy function rfunc.pro is written to call
; rfunc_hapke directly as a shell of this one.
;
; 9/27/2007, updated by JYL @UMd
;   1. Add keyword pfunc_form so that other forms of single-particle phase
;      function can be specified.
;   2. Correspondingly, add another keyword pfunc_param to pass the parameters 
;      of particle phase function.  Any parameters specified by this keyword 
;      will override the g parameters passed in the argument list.
; 12/17/2012, updated by JYL @PSI
;   1. add keyword aims to use the anisotropic multiple scattering form as
;      developed in Hapke (2002).
; 12/18/2012, updated by JYL @PSI
;   1. add keyword phi to include the porosity parameter as developed in
;      Hapke (2008).
;   2. add keywords bs0 and hs to specify the CBOE, as developed in Hapke
;      (2002).


function rfunc_hapke, w, g, b0, h, theta, i, e, alpha, cos=cos, pfunc_form=pfunc, pfunc_param=pfunc_param, aims=aims, phi=phi, bs0=bs0, hs=hs
  if n_params() ne 8 then begin
    print, 'Usage:  r = rfunc_hapke(w, g, b0, h, theta i, e, alpha[, /cos][, pfunc=#][, pfunc_param=array][, /aims][, phi=filling factor][, bs0=CBOE amplitude][, hs=CBOE widt])'
    return, 0
  endif

;  if size(phi,/type) eq 0 then phi=0

  if not keyword_set(cos) then begin
    mu0 = cos(i*!dtor)
    mu = cos(e*!dtor)
  endif else begin
    mu0 = i
    mu = e
  endelse

  if theta eq 0 then begin
    mu0e = mu0  &  mue = mu
    s = replicate(1d,n_elements(i))
  endif else begin
    psi = cal_psi(mu0,mu,alpha,/cos)
    mu0e = effinc(mu0,mu,psi,theta,/cos)
    mue = effemi(mu0,mu,psi,theta,/cos)
    s = sfunc(mu0,mu0e,mu,mue,psi,theta,/cos)
  endelse

  wterm = w/(4.*!dpi)*mu0e/(mu0e+mue)
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
    wterm *= kk
  endif

  ; cboe
  if size(bs0,/type) eq 0 then bs0=0
  if bs0 gt 0 then bs=1.+cboe(bs0,hs,alpha) else bs=1.

  ; single scattering part
  rs = rsingle(w,g,b0,h,mu0e,mue,alpha,/cos,pfunc_form=pfunc,pfunc_param=pfunc_param)
  ; multi scattering part
  rm = rmulti(w,g,mu0e,mue,alpha,/cos,pfunc_form=pfunc,pfunc_param=pfunc_param,aims=aims,phi=phi)

  return, wterm*(rs+rm)*s*bs

end
