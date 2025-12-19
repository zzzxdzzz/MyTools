;+
; NAME:
;
;  aki_int.pro
;
; PURPOSE:
;
;  Calculate Akimov integrated term with numerical interpolation
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = aki_int(pha[, /spline | /lsquadratic | /quadratic])
;
; INPUTS:
;
;  pha        - Phase angle in degrees
;
; OPTIONAL INPUTS:
;
;  spline, lsquadratic, quadratic  - Keywords to feed to IDL routine
;              `interpol`.  See docs of `interpol`.
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the Akimov integrated term
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
;  Program calculates integrated term by interpolating over the tabulated
;  values of Akimov integrated function from  numerical calculation
;  `diskint('akimov', findgen(181), nstep=1000)`, and saved in 'aki_int.tab'.
;  Spline interpolation is by default.
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  09/21/2018, created by JYL @PSI
;-

FUNCTION aki_int, pha, spline=spline, lsquadratic=lsquadratic, quadratic=quadratic

    IF NOT keyword_set(spline) AND  $
       NOT keyword_set(lsquadratic) AND  $
       NOT keyword_set(quadratic)  THEN spline=1
    findpro, 'aki_int', /noprint, dirlist=d
    readcol, d[0]+'aki_int.tab', pha0, aki0, /silent
    RETURN, interpol(aki0, pha0, pha, spline=spline, lsquadratic=lsquadratic, quadratic=quadratic)

END
