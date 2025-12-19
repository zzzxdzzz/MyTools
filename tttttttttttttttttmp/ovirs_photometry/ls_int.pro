;+
; NAME:
;
;  ls_int.pro
;
; PURPOSE:
;
;  Calculate Lommel-Seeliger integrated term
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = ls_int(pha)
;
; INPUTS:
;
;  pha        - Phase angle in degrees
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the Lommel-Seeliger integrated term
;      Phi = 1 - sin(pha/2) * tan(pha/2) * ln(cot(pha/4))
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

FUNCTION ls_int, pha
    pha2 = double(pha) * !dtor / 2
    IF n_elements(pha) EQ 1 THEN $
        IF pha2 EQ 0 THEN RETURN, 1.d  $
        ELSE RETURN, 1 + sin(pha2) * tan(pha2) * alog(tan(pha2/2))
    w1 = where(pha2 NE 0, complement=w2)
    phi = pha*0.d
    IF w1[0] NE -1 THEN phi[w1] = 1. + sin(pha2[w1]) * tan(pha2[w1]) * alog(tan(pha2[w1]/2))
    IF w2[0] NE -1 THEN phi[w2] = 1.d
    RETURN, phi
END
