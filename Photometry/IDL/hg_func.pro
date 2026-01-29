function hg_func, alpha, g1, g2, f

;+
; NAME:
;   hg_func.pro
;
; PURPOSE:
;   Calculate the HG form of single-particle phase function.
;
; DESCRIPTION:
;   This program calculate the single-particle phase function using either 
;   single-parameter form or double-parameter form Henyey-Greenstein function, 
;   depending on how many parameters are passed.  The single-term HG function 
;   has the form of:
;                              (1-g^2)
;      P_s(alpha) = ---------------------------
;                   (1+2*g*cos(alpha)+g^2)^1.5
;   where g is the single parameter, and alpha is phase angle.  The sign 
;   convension is such that g=0 means isotripic scattering, g>0 forward 
;   scattering, and g<0 backward scattering.  The double-term HG function takes 
;   the form of:
;      P_d(alpha) = (1-f)*P_s(alpha,g1) + f*P_s(alpha,g2)
;   where P_s is the single-term HG function, g1 and g2 are the two parameters, 
;   and f is the partition parameter.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   output = hg_func(alpha, g1[, g2, f])
;
; INPUTS:
;   alpha       - An array of phase angles that the phase function is 
;                 calculated.
;   g1          - The parameter for single-term HG function, or the first 
;                 parameter for double-term HG function.
;   g2          - The second parameter for double-term HG function.
;   f           - The partition parameter for double-term HG function.
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;   Program returns an array of the HG phase function calculated for input 
;   parameters under the input phase angles.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
; SIDE EFFECTS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   9/27/2007, created by JYL @UMd.  It is inherited from old function pfunc.pro
;
;-


on_error, 2

nparams = n_params()
if nparams ne 2 and nparams ne 4 then begin
  print, 'Usage: output = hg_func(alpha, g1[, g2, f])'
  return, 0.
endif

cosa = cos(alpha*!dtor)

pp = (1-g1*g1)/(1+2*g1*cosa+g1*g1)^1.5
if nparams eq 4 then begin
  pp2 = (1-g2*g2)/(1+2*g2*cosa+g2*g2)^1.5
  pp = (1-f)*pp+f*pp2
endif

return, pp

end
