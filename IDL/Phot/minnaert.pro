;+
; NAME:
;
;  minnaert.pro
;
; PURPOSE:
;
;  Calculate Minnaert disk-function
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  d = minnaert(inc, emi, k[, /deriv])
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;  k          - Exponential parameter
;
; OPTIONAL INPUTS:
;
;  deriv      - If set, then program returns disk function and the derivatives
;               w/r to parameter k in a 2-D array.  Default is to calculate
;               disk function only
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  By default, program returns the disk function for the input parameter k
;  and geometries in a 1-D array of M elements, where M is number of phase
;  angles.
;
;  If `deriv' is set, then program returns the disk function and derivatives
;  w/r to the parameter k in a 2-D array of dimensions 2xM, where [0,*] is the
;  disk function and [1,*] is the derivatives.
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
;  Minnaert disk-function: D = u0^k * u^(k-1)
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 15, 2017, created by JYL @PSI
;-

FUNCTION minnaert, inc, emi, k, deriv=deriv
    u0 = cos(inc * !dtor)
    u = cos(emi * !dtor)
    uu = u0*u
    d = u0*uu^(k-1)
    ; derivatives
    IF keyword_set(deriv) THEN BEGIN
        J = d*alog(uu)
        d = [transpose(d), transpose(j)]
    ENDIF

    RETURN, d
END
