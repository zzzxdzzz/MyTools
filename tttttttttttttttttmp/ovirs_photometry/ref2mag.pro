;+
; NAME:
;
;  ref2mag.pro
;
; PURPOSE:
;
;  Convert averaged reflectance to magnitude
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  mag = ref2mag(ref, radius, msun=-26.74)
;
; INPUTS:
;
;  ref - Average reflectance.  Note that geometric albedo = pi*`ref(0)`
;  radius  - Radius of target in km
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  referr  - Error associated with `ref`
;  magerr  - Returns eror associated with `mag`
;  msun - Solar magnitude corresponding to the band of `ref`, default is
;         V-band -26.74
;
; OUTPUTS:
;
;  Returns the magnitude
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
;-


function ref2mag, ref, radius, referr=referr, magerr=magerr, msun=msun
    IF NOT keyword_set(msun) THEN msun=-26.74
    mag = msun-2.5*alog10(ref*!pi*radius*radius/(1.496e8*1.496e8))
    IF keyword_set(referr) THEN magerr = 2.5*alog10(referr/ref+1)
    RETURN, mag
END
