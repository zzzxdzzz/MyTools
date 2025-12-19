;+
; NAME:
;
;  pholatlon.pro
;
; PURPOSE:
;
;  Calculate photometric latitude and longitude
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  latlon = pholatlon(inc, emi, pha[, /cos])
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
;  cos    - If set, then all angles are expressed in their cosines, including
;           the returned angles
;  lat, lon - Variables return the photometric latitude and longitude,
;           respectively, in degrees
;
; OUTPUTS:
;
;  If inputs are 1D arrays, then returns a Nx2 array with the photometric
;  latitude and longitude in deg or their cosines (if /cos is set).  If inputs
;  are high dimentional arrays, then each column in the returned array
;  correspond to flattened input arrays.
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
;  Kreslavsky et al., 2000, JGR 105, E8, 20,281-20,295
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  July 10, 2017, created by JYL @PSI
;-

FUNCTION pholatlon, inc, emi, pha, cos=cos, lat=lat, lon=lon

    IF keyword_set(cos) THEN BEGIN
        cosinc = [double(inc)]
        cosemi = [double(emi)]
        cospha = [double(pha)]
    ENDIF ELSE BEGIN
        cosinc = [cos(inc*!pi/180)]
        cosemi = [cos(emi*!pi/180)]
        cospha = [cos(pha*!pi/180)]
    ENDELSE
    sininc = sqrt(1.-cosinc*cosinc)
    sinemi = sqrt(1.-cosemi*cosemi)
    sinpha = sqrt(1.-cospha*cospha)

    lon = (coslat = cosinc*0.)
    w1 = where(cosemi NE 0, complement=w2)
    IF w1[0] NE -1 THEN BEGIN
        lon[w1] = atan(cosinc[w1]/cosemi[w1]-cospha[w1], sinpha[w1])
        coslat[w1] = (cosemi[w1]/cos(lon[w1]))<1.d
    ENDIF
    IF w2[0] NE -1 THEN BEGIN
        lon[w2] = 90.d
        coslat[w2] = 1.d
    ENDIF

    IF keyword_set(cos) THEN BEGIN
        lat = coslat
        lon = cos(lon)
    ENDIF ELSE BEGIN
        lat = acos(coslat) / !dtor
        lon = lon / !dtor
    ENDELSE

    np = n_elements(inc)
    RETURN, [[reform(lat,np)], [reform(lon,np)]]

END
