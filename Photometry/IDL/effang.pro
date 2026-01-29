function effang, x1, x2, psi0, theta, cos=cos

; The 2-in-1 version of effinc.pro and effemi.pro.  Function
; returns the effective angle of x1, given the two angles (i,e)
; as (x1,x2).  i.e, if want to compute effective incidence angle,
; then call this function as
;    ieff = effang(i,e,psi0,theta)
; if want to compute effective emission angle, call it as
;    eeff = effang(e,i,psi0,theta)
;
;
; Inputs:
;   x1, x2:  The incidence angle and emission angle
;   psi0:  The angle between incidence plane and the emission plane
;   theta: Global roughness parameter
;
; Outputs:
;   Function returns the effective angle of x1
;
; Keywords:
;   /cos:  If set, input i and e are actually cos(i) and cos(e),
;          or mu0 and mu.  And the output will also be the
;          cosines of effective incidence angle and effective
;          emission angle, i.e., mu0e and mue.
;
;
; History:
;
; 11/20/2003, created by JL
;

  if theta eq 0 then return, [x1]
  if keyword_set(cos) then cos=1 else cos=0

  if cos then begin
    mux1 = double(x1)  &  mux2 = double(x2)
  endif else begin
    mux1 = cos(x1*!dpi/180)  &   mux2 = cos(x2*!dpi/180)
  endelse
  sx1 = sqrt(1-mux1^2)  &  sx2 = sqrt(1-mux2^2)  ; sin(i) and sin(e)
  psi = psi0*!dpi/180
  tantheta = tan(theta*!dpi/180)
  cottheta = tantheta  &  cottheta[*] = double(1e38)
  w = where(abs(tantheta) gt 1e-37)
  if w[0] ne -1 then cottheta[w] = 1/tantheta[w]
  cotx1 = sx1  &  cotx1[*] = double(1e38)  &  cotx2 = cotx1
  w = where(sx1 gt 1e-37)  &  if w[0] ne -1 then cotx1[w] = mux1[w]/sx1[w]
  w = where(sx2 gt 1e-37)  &  if w[0] ne -1 then cotx2[w] = mux2[w]/sx2[w]

  e1b = mux1  &  e1s = e1b  &  e2b = e1b  & e2s = e1b
  f = mux1  &  g = mux1
  w1 = where(sx1 ge sx2)    ; in w1, x1 is the big angle, x2 is the small angle
  w2 = where(sx1 lt sx2)    ; in w2, x2 is the big angle, x1 is the small angle
  if w1[0] ne -1 then begin
    e1b[w1] = e1(cotx1[w1],cottheta[w1])  &  e1s[w1] = e1(cotx2[w1],cottheta[w1])
    e2b[w1] = e2(cotx1[w1],cottheta[w1])  &  e2s[w1] = e2(cotx2[w1],cottheta[w1])
    f[w1] = 1.
    g[w1] = -1.
  endif
  if w2[0] ne -1 then begin
    e1b[w2] = e1(cotx2[w2],cottheta[w2])  &  e1s[w2] = e1(cotx1[w2],cottheta[w2])
    e2b[w2] = e2(cotx2[w2],cottheta[w2])  &  e2s[w2] = e2(cotx1[w2],cottheta[w2])
    f[w2] = cos(psi[w2])
    g[w2] = 1.
  endif

  xi = 1./sqrt(1.+!dpi*tantheta^2)
  mue = xi*(mux1+sx1*tantheta*(f*e2b+g*sin(psi/2)^2*e2s)/(2-e1b-psi*e1s/!dpi))

  if cos then return, mue else return, acos(mue)*180/!dpi

end
