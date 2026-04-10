;+
; NAME:
;
;  akimov_model.pro
;
; PURPOSE:
;
;  Calculate the bidirectional reflectance based on Akimov model.  See
;  Shkuratov et al. 2011, PSS 59, 1326-1371
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  bdr = akimov_model(pha, inc, emi, a_ak, p1, p2, p3[, eta=1.][, /deriv][, /radf | /reff | /brdf])
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  pha        - Phase angle in degrees
;  a_ak       - Akimov albedo
;  p1, p2, p3 - Three phase function parameters
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  eta        - Optional Akimov disk-function parameter, default=1
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
;  derivatives w/r to the model parameters in a 2-D array of dimensions 5xM,
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
;  June 8, 2017, created by JYL @PSI
;-

FUNCTION akimov_model, pha, inc, emi, a_ak, p1, p2, p3, eta=eta, deriv=deriv, radf=radf, reff=reff, brdf=brdf

    IF NOT keyword_set(eta) THEN eta=1.0

    d = akimov(inc, emi, pha, eta=eta)
    falpha = expoly(pha, [0, p1, p2, p3], deriv=deriv)

    IF keyword_set(deriv) THEN BEGIN
        f = falpha[1,*]
        fder = falpha[2:*,*]
        r = a_ak * f * d
        j = [[transpose(r / a_ak)], $
             [transpose(replicate(a_ak,3) # d * fder)]]
        r = transpose([[transpose(r)], [j]])
    ENDIF ELSE r = a_ak * falpha * d

    RETURN, refquant(r, inc, radf=radf, reff=reff, brdf=brdf)

END
