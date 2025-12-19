;+
; NAME:
;
;  mcewen_model.pro
;
; PURPOSE:
;
;  Calculate the bidirectional reflectance based on the McEwen model
;  See McEwen (1991): McEwen, A.S., 1991.  Photometric functions for
;  photoclinometry and other applications.  Icarus 92, 298-311.  The
;  form adopted here uses an exponential function with a third polynomial
;  in the exponent as both the partition function and phase function.
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  bdr = mcewen_model(pha, inc, emi, a_mc, p1, p2, p3[, /deriv][, /radf | /reff | /brdf])
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  pha        - Phase angle in degrees
;  a_mc       - McEwen model albedo
;  p1, p2, p3 - Parameters for polynomial exponent in the partition function
;  p4, p5, p6 - Parameters for polynomial exponent in the phase function
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
;  derivatives w/r to the model parameters in a 2-D array of dimensions 8xM,
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
;    Use separate lommel_seeliger.pro, lambert.pro, and mcewen_partition.pro
;-

FUNCTION mcewen_model, pha, inc, emi, a_mc, p1, p2, p3, p4, p5, p6, deriv=deriv, radf=radf, reff=reff, brdf=brdf

    dalpha = mcewen(pha, inc, emi, p1, p2, p3, deriv=deriv)
    falpha = expoly(pha, [0, p4, p5, p6], deriv=deriv)

    IF keyword_set(deriv) THEN BEGIN
        f = falpha[1,*]
        fder = falpha[2:*,*]
        d = dalpha[0,*]
        dder = dalpha[1:*,*]
        r = a_mc * f * d
        j = [[transpose(r / a_mc)], $
             [transpose(replicate(a_mc,3) # d * fder)], $
             [transpose(replicate(a_mc,3) # f * dder)]]
        r = transpose([[transpose(r)], [j]])
    ENDIF ELSE r = a_mc * falpha * dalpha

    RETURN, refquant(r, inc, radf=radf, reff=reff, brdf=brdf)

END
