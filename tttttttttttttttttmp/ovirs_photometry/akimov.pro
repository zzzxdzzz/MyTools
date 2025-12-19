;+
; NAME:
;
;  akimov.pro
;
; PURPOSE:
;
;  Calculate Akimov disk function
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  d = akimov(inc, emi, pha[, eta=1.])
;
; INPUTS:
;
;  inc, emi, pha    - incidence, emission, and phase angles
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  eta    - Optional parameter eta.  Default is 1 for parameterless Akimov
;           disk function
;
; OUTPUTS:
;
;  Returns the Akimov disk function at (inc, emi, pha).
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
;  Eqs. 18, 19 in Shkuratov et al. 2011, PSS 59, 1326-1371
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  June 8, 2017, created by JYL @PSI
;-

FUNCTION akimov, inc, emi, pha, eta=eta

    IF not keyword_set(eta) THEN eta=1
    pha1 = pha*!dtor
    ;IF not keyword_set(ver) THEN ver=2
    ;if ver eq 2 then latlon = pholatlon(inc, emi, pha) $ &
    ;else latlon = pholatlon1(inc, emi, pha)
    latlon = pholatlon(inc, emi, pha, lat=lat, lon=lon)
    lat *= !dtor
    lon *= !dtor

    d = inc*0
    w1 = where((inc LE 90) AND (emi LE 90))
    IF w1[0] NE -1 THEN $
        d[w1] = cos(pha1[w1]/2) * cos(!pi/(!pi-pha1[w1])*(lon[w1]-pha1[w1]/2)) * cos(lat[w1])^(eta*pha1[w1]/(!pi-pha1[w1])) / cos(lon[w1])

    RETURN, d
END
