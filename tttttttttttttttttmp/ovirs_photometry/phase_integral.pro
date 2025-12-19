;+
; NAME:
;
;  phase_integral.pro
;
; PURPOSE:
;
;  Calculate phase integral
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  q = phase_integral(model, p[, /russell][, perr=array[, err=var]])
;
; INPUTS:
;
;  model      - A string to specify the photometric model name
;  p          - An array of photometric model parameters
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  russell  - If set, then use Russell's approximation rule (Russell, 1916)
;             where q ~ 2.17 Phi(54 deg).  Default is to perform a numerical
;             integration for phase function Phi(alpha) over [0, 180 deg].
;  perr     - The uncertainty of phase function parameters.  It could be a 1-D
;             array containing the 1-sigma uncertainty, or a 2-D array of
;             covariance matrix.  See individual integrated phase function
;             model for more details.
;
; OUTPUTS:
;
;  Returns the phase integral.  If at least one of the last 8 bit of flag is
;  set, then the phase integral is approximated by Russell's law, regardless
;  of keyword 'russell'
;
; OPTIONAL OUTPUTS:
;
;  flag     - 16-bit quality flag of output.  0 means good, 1-8 bits
;             mark non-fatal problems, and 9-16 bits mark fatal problem.
;             Bit flags:
;               1  - Calculation with Russell's law
;               2  - Math error detected (overflow, underflow, infinity
;                    etc.)
;               14 - Numerical integration error
;               15 - Non-monotonic phase function detected
;               16 - All zero photometric parameters received
;  err      - Returns the error estimate of phase integral
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
; PROCEDURE USED:
;
;  phase_func.pro
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : July 14, 2016, created by JYL @PSI
;  v1.1 : March 16, 2017, JYL
;    Generate and return quality flag for phase integral
;-

FUNCTION integral, pha, phi
    RETURN, 2*!dtor*total(phi*sin(pha*!dtor))
END

FUNCTION phase_integral, model, p, russell=russell, flag=flag, perr=perr, err=err
    COMMON modelpar, modell, pp

    flag = uint(0)

    ; check validity of parameters
    IF total(abs(p)) EQ 0 THEN BEGIN
        flag = flag OR ishft(1u, 15)
        err = 0.
        RETURN, 0.
    ENDIF

    catch, err
    IF err NE 0 THEN BEGIN
        result = 0
        flag = flag OR ishft(1u, 13)
        !except = ex
        err = 0.
        RETURN, result
    ENDIF

    ; suppress math warnings
    ex = !except
    !except = 0

    ; calculate phase integral
    pha = findgen(181)
    phi = phase_func(model, p, pha, perr=perr, err=phierr)
    IF check_math() NE 0 THEN flag = flag OR ishft(1u, 1)
    dphi =deriv(phi)
    IF max(dphi) GT 1e-6 THEN flag = flag OR ishft(1u, 14)

    ; calculate phase integral and error
    IF keyword_set(russell) OR (flag GE 512) THEN BEGIN
        flag = flag OR 1u
        result = 2.17 * phi[54] / p[0]
        IF keyword_set(perr) THEN err = phierr[54] / p[0]
    ENDIF ELSE BEGIN
        result = integral(pha,phi) / p[0]
        IF keyword_set(perr) THEN BEGIN
            rmax = integral(pha,phi+phierr)
            rmin = integral(pha,phi-phierr)
            err = (rmax-rmin)/(2*p[0])
        ENDIF
    ENDELSE

    ; restore math warning settings
    dummy = check_math()
    !except = ex

    RETURN, result
END
