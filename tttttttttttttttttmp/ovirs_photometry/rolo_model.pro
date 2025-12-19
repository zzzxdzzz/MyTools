;+
; NAME:
;
;  rolo_model.pro
;
; PURPOSE:
;
;  Calculate the bidirectional reflectance based on ROLO model with
;  the 7 parameter phase function form first adopted by Hillier et al.
;  (1999).
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  bdr = rolo_model(pha, inc, emi, c0, c1, a0, a1, a2, a3, a4[, /deriv][, /radf | /reff | /brdf])
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  pha        - Phase angle in degrees
;  c0, c1     - Exponential parameters for ROLO model
;  a0, a1, a2, a3, a4  - Polynomial parameters for ROLO model
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
;  v1.1 : Use separate lommel_seeliger.pro and rolo_phasefunc.pro
;-

FUNCTION rolo_model, pha, inc, emi, c0, c1, a0, a1, a2, a3, a4, deriv=deriv, radf=radf, reff=reff, brdf=brdf

  a0 = 0
  a1 = 0
  a2 = 0
  a3 = 0
  a4 = 0
  
  IF NOT keyword_set(eta) THEN eta=1.0

    d = akimov(inc, emi, pha, eta=eta)
    falpha = 10 ^ (c1 * pha / 2.5) ;* 2/(1+cos(pha* !dtor))

    IF keyword_set(deriv) THEN BEGIN
        f = falpha[1,*]
        fder = falpha[2:*,*]
        r = c0 * f * d
        j = [[transpose(r / c0)], $
             [transpose(replicate(c0,3) # d * fder)]]
        r = transpose([[transpose(r)], [j]])
    ENDIF ELSE r = c0 * falpha * d

    RETURN, refquant(r, inc, radf=radf, reff=reff, brdf=brdf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ROLO old model
;   d = lommel_seeliger(inc, emi)
;  falpha = rolo_phasefunc(pha, c0, c1, a0, a1, a2, a3, a4, deriv=deriv)
;
;    IF keyword_set(deriv) THEN BEGIN
;        f = falpha[0,*]
;        fder = falpha[1:*,*]
;        r = f * d / !pi
;        j = transpose(replicate(1.,7) # d * fder)
;        r = transpose([[transpose(r)], [j]])
;    ENDIF ELSE r = falpha * d / !pi
;
;    RETURN, refquant(r, inc, radf=radf, reff=reff, brdf=brdf)

END
