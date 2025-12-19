;+
; NAME:
;
;  broadcast.pro
;
; PURPOSE:
;
;  Implement Python-like broadcast rule to input array
;
; CATEGORY:
;
;  Facility
;
; CALLING SEQUENCE:
;
;  b = broadcast(a, dim)
;
; INPUTS:
;
;  a     - Input number or array to be broadcasted
;  dim   - The dimension of output array
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Program returns array `b` that is input `a` broadcasted to the
;  dimension `dim`.  For scalar or one-element array, program will just
;  return a scalar.
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
;  12/14/2017, created by JYL @PSI
;-

FUNCTION broadcast, a, dim

    adim = size(a, /dim)
    IF adim[0] EQ 0 THEN RETURN, a  ; do nothing for scalars
    IF n_elements(a) EQ 1 THEN RETURN, a[0]  ; return a scalar for one-element array

    nadim = n_elements(adim)
    ndim = n_elements(dim)
    IF nadim GT ndim THEN message, 'ERROR: Input array `a` has more dimensions than output dimensions'

    ; check dimension validity
    FOR i=1, nadim DO IF adim[-i] NE dim[-i] THEN message, 'ERROR: Incompatable dimensions for broadcast'

    af = a[*]
    na = n_elements(af)
    nb = 1
    FOR i=0, ndim-1 DO nb *= dim[i]
    bf = fltarr(nb)
    factor = nb/na
    FOR i=0, factor-1 DO bf[i:*:factor] = af
    RETURN, reform(bf, dim)

END
