;+
; NAME:
;
;  mcewen.pro
;
; PURPOSE:
;
;  Calculate McEwen disk function.  Use partition function
;      L(alpha) = exp(beta*alpha + gamma*alpha^2 + delta*alpha^3)
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  d = mcewen(pha, inc, emi, p1, p2, p3[, /deriv])
;
; INPUTS:
;
;  pha        - Phase angle in degrees
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  p1, p2, p3 - Parameter for partition function
;
; OPTIONAL INPUTS:
;
;  deriv      - If set, then program returns disk function and the derivatives
;               w/r to model parameters in a 2-D array.  Default is to
;               calculate disk function only
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  By default, program returns the disk function for the input parameters
;  and geometries in a 1-D array of M elements, where M is number of phase
;  angles.
;
;  If `deriv' is set, then program returns the disk function and derivatives
;  w/r to the parameters in a 2-D array of dimensions 4xM, where [0,*] is the
;  disk function and [1,*] is the derivatives.
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
;  June 10, 2017, created by JYL
;-

FUNCTION mcewen, pha, inc, emi, p1, p2, p3, deriv=deriv

    Lalpha = expoly(pha, [0, p1, p2, p3], deriv=deriv)
    d_ls = lommel_seeliger(inc, emi)
    d_lb = lambert(inc)
    IF keyword_set(deriv) THEN BEGIN
        l = Lalpha[1,*]
        d = 2 * l * d_ls + (1-l) * d_lb
        lder = Lalpha[2:*,*]
        dder = 2 * d_ls - d_lb
        lder *= replicate(1.,3) # dder
        d = transpose([[transpose(d)],[transpose(lder)]])
    ENDIF ELSE d = 2 * Lalpha * d_ls + (1-Lalpha) * d_lb

    RETURN, d

END
