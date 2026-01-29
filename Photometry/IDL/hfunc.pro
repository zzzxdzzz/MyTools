
; Approximation to Chandrasekhar's H function
; Hapke 1981
; h(x;w) = (1+2*x)/(1+2*x*sqrt(1-w)), first order approximation

function hfunc_81, w, x
;  nx = n_elements(x) & nw = n_elements(w)
  return, (1+2*x)/(1+2*x*sqrt(1-(w<1)))
end


; New Approximation to Chandrasekhar's H function
; Hapke, 2002, Icarus 157, 523
; h(x;w) = [1-w*x*(r0+(1-2*r0*x)/2*ln((1+x)/x))]^(-1)
;          r0 = (1-sqrt(1-w))/(1+sqrt(1-w))
; 7/31/2012, modified by JYL @PSI:
;  Corrected a bug when w is a vector but x is a scaler.  Now both w
;  and x can be vectors or a 1-element scaler.
function hfunc_02, w, x

  nw = n_elements(w)
  nx = n_elements(x)
  if nw ne nx and min([nw,nx]) gt 1 then begin
    message,/con, 'ERROR: Number of elements of imput parameters do not match!'
    return, 0.
  endif
  x1 = x
  w1 = w
  if max([nw,nx]) gt 1 then begin
    if nw eq 1 then w1=replicate(w,nx)
    if nx eq 1 then x1=replicate(x,nw)
  endif

  h = double(x1)
  z=where(x1 eq 0,ct)
  if ct gt 0 then h[z]=1
  z=where(x1 ne 0,ct)
  if ct gt 0 then begin
    gamma = sqrt(1-(w1<1))
    r0 = (1-gamma)/(1+gamma)
    h[z] = 1./(1-w1[z]*x1[z]*(r0+(1-2*r0*x1[z])*alog((1+x1[z])/x1[z])/2))
 endif

  return, h
end


; Approximation to Chandrasekhar's H function

function hfunc, w, x

  if n_params() ne 2 then begin
    message, /con,'Usage: hfunc(w,x)'
    return,0
  endif

;  return, hfunc_81(w,x)    ; use old H function as in Hapke 1981
  return, hfunc_02(w,x)    ; use new H function as in Hapke 2002
end
