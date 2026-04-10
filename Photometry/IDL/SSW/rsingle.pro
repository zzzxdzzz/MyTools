; Singly scattered reflectance
; 9/27/2007, updated by JYL @UMd
;   1. Add keyword pfunc_form so that other forms of single-particle phase function 
;      can be specified.
;   2. Correspondingly, add another keyword pfunc_param to pass the parameters 
;      of particle phase function.  Any parameters specified by this keyword 
;      will override the g parameters passed in the argument list.
; 12/18/2012, updated by JYL @PSI
;   1. removed the 4*pi/w*mu0/(mu0+mu) term, so that the rsingle part only
;      contains pfunc()*(1+B), where B is the SHOE term.
;

function rsingle, w, g, b0, h, i, e, alpha, cos=cos, pfunc_form=form, pfunc_param=pfunc_param

  if n_params() ne 7 then begin
    print, 'Usage: r = rsingle(w, g, b0, h, i, e, alpha[, /cos][, pfunc=#][, pfunc_param=array])'
    return, 0.
  endif

  if not keyword_set(cos) then cos=0

  if not cos then begin
    mu0 = cos(i/180.*!dpi)
    mu = cos(e/180.*!dpi)
  endif else begin
    mu0 = double(i)
    mu = double(e)
  endelse

  rs = pfunc(g,alpha,form=form,parameters=pfunc_param)
  if b0 gt 0 then rs *= 1+bfunc(b0,h,alpha)

  return, rs

end
