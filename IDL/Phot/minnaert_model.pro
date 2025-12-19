;+
; NAME:
;
;  minnaert_model.pro
;
; PURPOSE:
;
;  Calculate the bidirectional reflectance based on Minnaert model
;  with a 10-based exponential phase function where the exponent is
;  a third order polynomial.  See Takir et al. (2015).
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  bdr = minnaert_model(pha, inc, emi, a_m, p1, p2, p3, k0, b[, /deriv][, /radf | /reff | /brdf])
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  pha        - Phase angle in degrees
;  a_m        - Minnaert albedo
;  p1, p2, p3 - Parameter for polynomial exponent in the phase function
;  k0, b      - Parameter for the linear Minnaert k parameter
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  deriv      - If set, then program returns model reflectance and the
;               derivatives w/r to model parameters in a 2-D array.  Default
;               is to calculate model reflectance only
;  radf       - If set, then program returns RADF
;  reff       - If set, then program returns REFF
;  brdf       - If set, then program returns BRDF
;  NOTE: If more than one keyword is set, then the one appears on this list
;        earlier will override the one later.  E.g., if both `radf' and `brdf'
;        are set, then program will return `radf'; and if `reff' and `brdf' are
;        both set, then program will return `reff'.
;
; OUTPUTS:
;
;  By default, program returns the reflectance quantity for the input
;  photometric parameters and geometries with the specified model in a 1-D
;  array of M elements, where M is number of scattering geometries in
;  (pha, inc, emi).
;
;  If `deriv' is set, then program returns the model reflectance and
;  derivatives w/r to the model parameters in a 2-D array of dimensions 7xM,
;  where [0,*] is the model reflectances and [1:*,*] is the derivatives
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
;  v1.1 : March 15, 2017, JYL
;    Use separate minnaert.pro, minnaert_phasefunc.pro
;-

FUNCTION minnaert_model, pha, inc, emi, a_m, p1, p2, p3, k0, b, deriv=deriv, radf=radf, reff=reff, brdf=brdf

    ; minnaert model
    falpha = minnaert_phasefunc(pha, p1, p2, p3 ,deriv=deriv)
    kalpha = poly(pha, [k0, b])
    dalpha = minnaert(inc, emi, kalpha, deriv=deriv)

    IF keyword_set(deriv) THEN BEGIN
        f = falpha[0,*]
        fder = falpha[1:*,*]
        d = dalpha[0,*]
        dder = dalpha[1,*]
        r = a_m * f * d
        j = [[transpose(r / a_m)], $
             [transpose(replicate(a_m,3) # d * fder)], $
             [transpose(a_m * f * dder)], $
             [transpose(a_m * f * dder * pha)]]
        r = transpose([[transpose(r)], [j]])
    ENDIF ELSE r = a_m * falpha * dalpha

    RETURN, refquant(r, inc, radf=radf, reff=reff, brdf=brdf)

END
