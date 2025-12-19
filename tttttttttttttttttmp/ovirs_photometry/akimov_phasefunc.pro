;+
; NAME:
;
;  akimov_phasefunc.pro
;
; PURPOSE:
;
;  Calculate Akimov phase function
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  f = akimov(pha, m, u1, u2)
;
; INPUTS:
;
;  pha    - phase angle (deg)
;  m, u1, u2 - model parameters
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the Akimov phase function f(pha; m, u1, u2)
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; SIDE EFFECTS:
;
;
; RESTRICTIONS:
;
;
; PROCEDURE USED:
;
;  Eq. 21 in Shkuratov et al. 2011, PSS 59, 1326-1371
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  June 8, 2017, created by JYL @PSI
;-

FUNCTION akimov_phasefunc, pha, m, u1, u2
    RETURN, (exp(-u1*pha) + m*exp(-u2*pha)) / (1+m)
END
