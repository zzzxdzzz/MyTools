;+
; NAME:
;
;  lambert_int.pro
;
; PURPOSE:
;
;  Calculate Lambertian integrated term
;
; CATEGORY:
;
;  Photometric model
;
; CALLING SEQUENCE:
;
;  Phi = lambert_int(pha)
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
;      Phi = sin(pha) + (pi - pha) * cos(pha)
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

FUNCTION lambert_int, pha
    pha1 = double(pha) * !dtor
    RETURN, (sin(pha1) + (!dpi - pha1) * cos(pha1)) / !dpi
END
