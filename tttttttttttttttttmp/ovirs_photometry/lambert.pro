;+
; NAME:
;
;  lambert.pro
;
; PURPOSE:
;
;  Calculate Lambert disk-function
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  d = lambert(inc)
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the Lambert disk-function mu_0
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
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 15, 2017, created by JYL @PSI
;-

FUNCTION lambert, inc
    RETURN, cos(inc * !dtor)
END
