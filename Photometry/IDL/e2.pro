; Support functions for computing i_eff and e_eff, all angles
; are in rad, NOT deg
; Ref. Hapke 1993, Eq. 12.45c
;
; 9/13/2011, modified by JYL @UMd
;   * check the range of exponential and set 0 and infinity for the
;   power term less than -100 and greater than 100, respectively
;   * add on_error,2

function e2, cotx, cottheta


  power = -1d/!dpi*double(cottheta)*cottheta*double(cotx)*cotx
  e2 = power*0d
  w = where(abs(power) le 100,ct)
  if ct gt 0 then e2[w] = exp(power[w])
  w = where(power lt -100,ct)
  if ct gt 0 then e2[w]=0.
  w = where(power gt 100,ct)
  if ct gt 0 then e2[w]=!values.d_infinity

  return, e2
end
