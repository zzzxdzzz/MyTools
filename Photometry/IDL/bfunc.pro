; Opposition effect function as in Bowell 1989 and Helfenstein 1989
; B = B0/(1+tan(alpha/2)/h)
;
; 11/24/2003, change a little bit to avoid the "divided by zero" error.
;
; 12/10/2003, revise for the possible case that the input (B0,h) and alpha 
;   have different dimentions

function bowellbfunc, b1, h1, alpha
  nb = n_elements(b1)
  nh = n_elements(h1)
  nalpha = n_elements(alpha)
  nmax = max([nb,nh,nalpha])
  if nb ne nmax then b0=replicate(b1,nmax) else b0=b1
  if nh ne nmax then h =replicate(h1,nmax) else h=h1
  if nalpha ne nmax then alpha0=replicate(alpha,nmax) else alpha0=alpha

  b = double(b0)  &  b[*] = 0
  alpha0 = abs(alpha0) mod 360
  w = where(alpha0 gt 180) & if w[0] ne -1 then alpha0[w]=360-alpha0[w]
  alpha0 = alpha0*!dpi/180
  w = where(h ne 0)
  if w[0] ne -1 then begin
    b[w] = b0[w]/(1+tan(alpha0[w]/2.)/h[w])
  endif else begin
    w = where(h eq 0)
    if w[0] ne -1 then begin
      w1 = where(alpha[w] eq 0)
      if w1[0] ne -1 then b[w[w1]] = b0[w[w1]]
    endif
  endelse
  return, b
end


; Opposition effect function as in Hapke 1981
; B = B0*(1-tan|alpha|*(3-exp(-h/tan|alpha|))*(1-exp(-h/tan|alpha|))/(2*h))
;  |alpha| <= pi/2

function hapkebfunc, b0, h, alpha
  alpha1 = abs(alpha) mod 360
  w = where(alpha1 gt 180) & if w[0] ne -1 then alpha1[w]=360-alpha1[w]
  alpha1 = alpha1/180.*!dpi & n_alpha = n_elements(alpha1)
  if n_alpha ne 1 then begin ;one parameter set with multiple phase angles
    w1 = where(alpha1 ne 0) & expterm=alpha1 & expterm[*]=0.
    if w1[0] ne -1 then expterm[w1] = exp(-h/tan(alpha1[w1]))
    w = where(alpha1 lt !dpi/2.) & b=alpha1 & b[*]=0
    if w[0] ne -1 then $
      b[w]=b0*(1-tan(alpha1[w])/2./h*(3-expterm[w])*(1-expterm[w]))
  endif else begin ; one phase angle with possibly multiple parameter sets
    b = b0 & b[*] = 0
    if alpha1 lt !dpi/2 then begin  ; if alpha1 ge pi/2, then b is 0
      expterm = h
      if alpha1 eq 0 then expterm[*]=0 else expterm=exp(-h/tan(alpha1))
      b = b0*(1-tan(alpha1)/2./h*(3-expterm)*(1-expterm))
    endif
  endelse
  return, b
end


; Select a B function

function bfunc, b0, h, alpha
  return, bowellbfunc(b0,h,alpha)  ; as in Bowell et al 1989
;  return, hapkebfunc(b0,h,alpha)   ; as in Hapke 1981
end


