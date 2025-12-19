;+
; NAME:
;
;  mag2ref.pro
;
; PURPOSE:
;
;  Convert magnitude to averaged reflectance
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  ref = mag2ref(ref, radius, msun=-26.74)
;
; INPUTS:
;
;  mag - Magnitude of target
;  radius  - Radius of target in km
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  magerr  - Error associated with `mag`
;  referr  - Returns error associated with `ref`
;  msun - Solar magnitude corresponding to the band of `ref`, default is
;         V-band -26.74
;
; OUTPUTS:
;
;  Returns the average reflectance.  Note that geometric albedo = pi*`ref(0)`
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


function mag2ref, mag, radius, magerr=magerr, referr=referr, msun=msun
    IF NOT keyword_set(msun) THEN msun=-26.74
    ref = 10^((msun-mag)*0.4)*1.496e8*1.496e8/(!pi*radius*radius)
    IF keyword_set(magerr) THEN referr = (10^(0.4*magerr)-1)*ref
    RETURN, ref
END
