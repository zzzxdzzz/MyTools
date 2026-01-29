; Compute the macroscopic roughness correction coefficient
; s(i,e,alpha,theta)
; Ref. Hapke 1984
;
; History:
;
;   11/23/2003, found a discrepency in compute the mu_e(0) and mu_0e(0) in
;       Hapke's 1993 book.  Coded it into program, waiting for the final
;       result.
;
;   12/7/2003, it turned out that the equations of mu_e(0) and mu_0e(0) in
;       Hapke's 1993 book are correct.  The possible reason to cause S>1 is
;       that almost all equations are approximated, therefore the very minor
;       ill behavior of the 2nd order derivations of the component equations
;       causes the <=0.4% excess of S from 1.  Changed all mu_e(0) and
;       mu_0e(0) to the Hapke's book equations.
;
;   5/3/2004, updated by JL: eliminate a bug happened when input i, e, and
;    psi have different elements.


function sfunc, i, ieff, e, eeff, psi, theta, cos=cos

  if theta eq 0 then return, replicate(1d,n_elements(i))
  if not keyword_set(cos) then cos=0

  n_i = n_elements(i)
  n_e = n_elements(e)
  n_if = n_elements(ieff)
  n_ef = n_elements(eeff)
  n_psi = n_elements(psi)
  nn = max([n_i,n_e,n_if,n_ef,n_psi])

  if cos then begin   ; inputs are mu and mu0
    mu0 = double(i)  &  mu = double(e)
    mu0e = double(ieff)  &  mue = double(eeff)
  endif else begin    ; inputs are angles in deg
    mu0 = cos(i*!dpi/180)  &  mu = cos(e*!dpi/180)
    mu0e = cos(ieff*!dpi/180)  &  mue = cos(eeff*!dpi/180)
  endelse
  si = sqrt(1-mu0^2)  &  se = sqrt(1-mu^2)

  tantheta = tan(theta*!dpi/180)  &  cottheta = 1/tantheta
  xi = 1./sqrt(1.+!dpi*tantheta^2)
  fpsi = -2*tan(psi*!dpi/360)

  if nn gt 1 then begin
    if n_psi eq 1 then fpsi=replicate(fpsi,nn)
    if n_i eq 1 then si=replicate(si,nn)
    if n_e eq 1 then se=replicate(se,nn)
  endif

  w1 = where(fpsi lt -100,ct1)
  if ct1 gt 0 then fpsi[w1]=0.
  w2 = where(abs(fpsi) le 100,ct2)
  if ct2 gt 0 then fpsi[w2]=exp(fpsi[w2])
  w3 = where(fpsi gt 100,ct3)
  if ct3 gt 0 then fpsi[w3]=!values.d_infinity

;  coti = si  &  coti[*] = 1e38  &  cote = coti
;  w = where(si ne 0)  &  if w[0] ne -1 then coti[w] = mu0[w]/si[w]
;  w = where(se ne 0)  &  if w[0] ne -1 then cote[w] = mu[w]/se[w]
;  e1e = e1(cote,cottheta)  &  e1i = e1(coti,cottheta)
;  e2e = e2(cote,cottheta)  &  e2i = e2(coti,cottheta)
;  mu0e0 = effang(mu0,mu,0,theta,/cos)
;  mue0 = effang(mu,mu0,0,theta,/cos)


;  mu0e0 = effang0(mu0, theta, /cos)
;  mue0 = effang0(mu, theta, /cos)
  mu0e0 = effinc(mu0,mu,0,theta,/cos)
  mue0 = effemi(mu0,mu,0,theta,/cos)

  s = psi*0d  &  s[*] = 1d
  w1 = where(si le se)  &  w2 = where(si gt se)
  if w1[0] ne -1 then begin
    s[w1] = mu0[w1]/mu0e0[w1]
  endif
  if w2[0] ne -1 then begin
    s[w2] = mu[w2]/mue0[w2]
  endif
  s = mue/mue0*mu0/mu0e0*xi/(1-fpsi+fpsi*xi*s)

  return, s
end
