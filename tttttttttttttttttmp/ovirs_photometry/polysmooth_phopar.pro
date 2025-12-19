;+
; NAME:
;
;  polysmooth_phopar.pro
;
; PURPOSE:
;
;  Smooth out photometric parameters with polynomial fitting
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  par1 = polysmooth_phopar(par[, x=x][, degree=3][,thresh=3]
;                              [, polypar=variable]
;                              [, yerr=variable][, yband=variable])
;
; INPUTS:
;
;  par      - Photometric parameters to be smoothed
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  x        - The x-axis of the parameters.  Usually it is wavelength.  If
;             absent, then the parameters will be assumed to be uniform
;             along x-axis
;  degree   - Degree of the polynomial fitting
;  thresh   - Sigma threshold for outlier rejection.  Default is 3-sigma.  Set
;             it to 0 will not reject outliers before polynomial fitting
;
; OUTPUTS:
;
;  Returns the smoothed photometric parameters.  Note that the first
;  photometric parameter, which is usually albedo in most empirical models
;  is not smoothed.  The albedo is cancelled out in photometric corrections
;  with linear photometric model (usual case for dark surface).
;
; OPTIONAL OUTPUTS:
;
;  polypar  - The best-fit polynomial parameters.  It has size D+1 x Npar-1,
;             where D is the degree of polynommial fit, and Npar is the number
;             of photometric parameters
;  yerr     - A named variable returns the `yerror` of all bands from
;             polynomial fit.  See IDL routine `poly_fit`
;  yband    - A named variable returns the `yband` of all bands from
;             polynomial fit.  See IDL routine `poly_fit`
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
;   goodpoly.pro, which in turn uses trimrank.pro and badpar.pro.  All is from
;       Marc Buie's library downloaded from his website at SWRI
;       (https://www.boulder.swri.edu/~buie/idl/), and included in this OVIRS
;       photometry software package.
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;-


FUNCTION polysmooth_phopar, par, x=x, degree=degree, polypar=polypar, thresh=thresh, yerr=yerr, yband=yband

    IF NOT keyword_set(degree) THEN degree = 3
    IF size(thresh,/type) EQ 0 THEN thresh = 3

    sz = size(par, /dim)
    IF size(x,/type) EQ 0 THEN x = findgen(sz[0])

    par1 = fltarr(sz)
    par1[*,0] = par[*,0]
    yerr = fltarr(sz[1]-1)
    yband = fltarr([sz[0],sz[1]-1])
    polypar = fltarr(degree+1, sz[1]-1)
    flag = total(par,2)
    good = where(flag NE 0)
    bad = flag EQ 0
    IF good[0] NE -1 THEN BEGIN
        FOR i=1, sz[1]-1 DO BEGIN
            IF keyword_set(thresh) THEN BEGIN
                  fit = goodpoly(x, par[*,i], degree, thresh, newx, newy, newerr,bad=bad)
                  good1 = where(bad EQ 0)
                  good1_cut = good1[where(good1 LT 1223)]
                  fit = poly_fit(x[good1_cut], par[good1_cut,i], degree, yband=yb, yerror=ye)
                  yband[good1_cut, i-1] = yb
               ENDIF ELSE BEGIN
                  good_cut = good[where(good LT 1223)]
                fit = poly_fit(x[good_cut], par[good_cut,i], degree, yband=yb, yerror=ye)
                yband[good_cut, i-1] = yb
            ENDELSE
            par1[good,i] = poly(x[good], fit)
            yerr[i-1] = ye
            polypar[*,i-1] = fit
        ENDFOR
    ENDIF

    RETURN, par1

END
