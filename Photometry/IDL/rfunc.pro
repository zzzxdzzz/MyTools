; A dummy function to call rfunc_hapke for the purpose of backward
; compatability of rfunc_hapke, whose name used to be rfunc.pro
;
; 10/13/2004, JL, UMd, Create
;
; 9/27/2007, updated by JYL @UMd
;   1. Add keyword pfunc_form so that other forms of single-particle phase function 
;      can be specified.
;   2. Correspondingly, add another keyword pfunc_param to pass the parameters 
;      of particle phase function.  Any parameters specified by this keyword 
;      will override the g parameters passed in the argument list.
;
; 12/18/2012, updated by JYL @PSI
;   1. add _extra keyword mechnism to accomodate future changes in rfunc_hapke.pro


function rfunc, w, g, b0, h, theta, i, e, alpha, cos=cos, pfunc_form=pfunc, pfunc_param=pfunc_param, _ref_extra=_extra

  if n_params() ne 8 then begin
    print, 'Usage:  r = rfunc(w, g, b0, h, theta i, e, alpha[, /cos][, pfunc=#][, pfunc_param=array][, /aims])'
    return, 0
  endif

  return, rfunc_hapke(w, g, b0, h, theta, i, e, alpha, cos=cos,pfunc_form=pfunc,pfunc_param=pfunc_param,_extra=_extra)

end
