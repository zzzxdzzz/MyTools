; Compute the effective incidence angle
; It takes parameters (i, e, psi, theta), returns the
; effective incidence angle.
; Based on Hapke theory of light scattering out of a rough
; surface (Hapke 1984)
;
; 5/3/2004, updated by JL: eliminate a bug happened when input i, e, and
;    psi0 have different elements.
; 9/12/2011, modified by JYL @UMd;
;   changed the infinity value from 1e38 to !values.d_infinity
;

function effinc, i, e, psi0, theta, cos=cos
; if set keyword 'cos', then all i, e, and returned effective
; incident angle are the cosines of the corresponding angles

  if theta le 0 then return, i
  if not keyword_set(cos) then cos=0

  n_i = n_elements(i)
  n_e = n_elements(e)
  n_psi = n_elements(psi0)
  nn = max([n_i,n_e,n_psi])

  if cos then begin   ; inputs are mu and mu0
    mu0 = i  &  mu = e
  endif else begin    ; inputs are angles in deg
    mu0 = cos(i*!dpi/180)  &  mu = cos(e*!dpi/180)
  endelse
  si = sqrt(1-mu0^2)  &  se = sqrt(1-mu^2)  ; sin(i) and sin(e)
  psi = psi0*!dpi/180   ; unit convertion
  if nn gt 1 then begin
    if n_i eq 1 then si=replicate(si,nn)
    if n_e eq 1 then se=replicate(se,nn)
    if n_psi eq 1 then psi=replicate(psi,nn)
  endif
  tantheta = tan(theta*!dpi/180)  &  cottheta = 1/tantheta
  coti = si  &  coti[*] = !values.d_infinity  &  cote = coti
  w = where(si ne 0)  &  if w[0] ne -1 then coti[w] = mu0[w]/si[w]
  w = where(se ne 0)  &  if w[0] ne -1 then cote[w] = mu[w]/se[w]
  e1e = e1(cote,cottheta)  &  e1i = e1(coti,cottheta)
  e2e = e2(cote,cottheta)  &  e2i = e2(coti,cottheta)

  mu0e = si*0d
  w1 = where(si le se, complement=w2)
  if w1[0] ne -1 then begin   ; case i <= e (Eq.46)
    mu0e[w1] = (cos(psi[w1])*e2e[w1]+sin(psi[w1]/2)^2*e2i[w1])/(2-e1e[w1]-psi[w1]*e1i[w1]/!dpi)
  endif
  if w2[0] ne -1 then begin   ; case i >= e (Eq.50)
    mu0e[w2] = (e2i[w2]-sin(psi[w2]/2)^2*e2e[w2])/(2-e1i[w2]-psi[w2]*e1e[w2]/!dpi)
  endif

  xi = 1./sqrt(1.+!dpi*tantheta^2)
  mu0e = (mu0+si*tantheta*mu0e)*xi

  if cos then return, mu0e else return, acos(mu0e)/!dpi*180

end
