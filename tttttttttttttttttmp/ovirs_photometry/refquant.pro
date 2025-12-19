;+
; NAME:
;
;  refquent.pro
;
; PURPOSE:
;
;  Choose the reflectance quantity from input bidirectional reflectance.
;  See Reflectance_Albedo_Quantities document.
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  quantity = refquant(bdr[, inc][, /radf | /reff | /brdf]
;                         [, from='radf'|'reff'|'brdf'])
;
; INPUTS:
;
;  bdr        - Bidirectional reflectance
;
; OPTIONAL INPUTS:
;
;  inc        - Incidence angle in degrees
;
; KEYWORD PARAMETERS:
;
;  radf       - If set, then program returns RADF
;  reff       - If set, then program returns REFF
;  brdf       - If set, then program returns RADF
;  from       - Set the unit of input quantity: 'radf', 'reff', 'brdf'
;  NOTE: For REFF and BRDF, optional input `inc' has to be set.  Otherwise
;        program will generate an error.
;  NOTE: If more than one keyword is set, then the order of override is
;        `radf'  >  `reff'  >  `brdf'
;        E.g., if both `radf' and `brdf' are set, then program will return
;        `radf'; or if `reff' and `brdf' are both set, then program will
;        return `reff'.
;  NOTE: If none of the keyword is set, then program will return
;        bdr(Bidirectional reflectance)
;
; OUTPUTS:
;
;  Program returns the bidirectional reflectance quantity of choice.
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : May 20, 2016, created by JYL @PSI
;  v2.0 : July 20, 2016, modified by JYL @PSI
;    Added keyword `from' to control the unit of input quantity
;
;-

FUNCTION refquant, bdr1, inc1, radf=radf, reff=reff, brdf=brdf, from=from

    ; broadcast inc1 along the last axis of bdr1
    inc = broadcast(inc1, size(bdr1, /dim))

    bdr = bdr1
    IF NOT keyword_set(from) THEN from = 'bdr'

    ; convert input quantity to bidirectional reflectance
    IF from EQ 'radf' THEN BEGIN
        bdr /= !pi
    ENDIF ELSE IF from EQ 'reff' THEN BEGIN
        IF n_params() < 2 THEN BEGIN
            message, 'incidence angle not provided'
            RETURN, -1
        ENDIF
        u0 = cos(inc * !dtor)
        bdr *= u0/!pi
    ENDIF ELSE IF from EQ 'brdf' THEN BEGIN
        IF n_params() < 2 THEN BEGIN
            message, 'incidence angle not provided'
            RETURN, -1
        ENDIF
        u0 = cos(inc * !dtor)
        bdr *= u0
    ENDIF

    ; convert output
    IF keyword_set(radf) THEN BEGIN
        RETURN, !pi * bdr
    ENDIF ELSE IF keyword_set(reff) THEN BEGIN
        IF n_params() < 2 THEN BEGIN
            message, 'incidence angle not provided'
            RETURN, -1
        ENDIF
        u0 = cos(inc * !dtor)
        RETURN, !pi * bdr / u0
    ENDIF ELSE IF keyword_set(brdf) THEN BEGIN
        IF n_params() < 2 THEN BEGIN
            message, 'incidence angle not provided'
            return, -1
        ENDIF
        u0 = cos(inc * !dtor)
        RETURN, bdr / u0
    ENDIF ELSE RETURN, bdr

END
