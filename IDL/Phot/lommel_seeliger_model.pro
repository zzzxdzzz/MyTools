;+
; NAME:
;
;  lommel_seeliger_model.pro
;
; PURPOSE:
;
;  Calculate the bidirectional reflectance based on Lommel-Seeliger model
;  with an exponential phase function where the exponent is a third order
;  polynomial function.  See Takir, D. et al., 2015.  Photometric models of
;  disk-integrated observations of the OSIRIS-REx target asteroid (101955)
;  Bennu.  Icarus 252, 393-399.
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  bdr = lommel_seeliger_model(pha, inc, emi, a_ls, p1, p2, p3[, /deriv][, /radf | /reff | /brdf])
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  pha        - Phase angle in degrees
;  a_ls       - Lommel-Seeliger albedo
;  p1, p2, p3 - Three phase function parameters
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
;  v1.0 : May 20, 2016, created by JYL @PSI
;  v1.1 : March 15, 2017, JYL
;    Use separate lommel_seeliger.pro and ls_phasefunc.pro
;-

FUNCTION lommel_seeliger_model, pha, inc, emi, a_ls, p1, p2, p3, deriv=deriv, radf=radf, reff=reff, brdf=brdf

    d = lommel_seeliger(inc, emi)
    falpha = expoly(pha, [0, p1, p2, p3], deriv=deriv)

    IF keyword_set(deriv) THEN BEGIN
        f = falpha[1,*]
        fder = falpha[2:*,*]
        r = a_ls * f * d
        j = [[transpose(r / a_ls)], $
             [transpose(replicate(a_ls,3) # d * fder)]]
        r = transpose([[transpose(r)], [j]])
    ENDIF ELSE r = a_ls * falpha * d

    RETURN, refquant(r, inc, radf=radf, reff=reff, brdf=brdf)

END

