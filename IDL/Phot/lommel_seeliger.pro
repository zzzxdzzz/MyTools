;+
; NAME:
;
;  lommel-seeliger.pro
;
; PURPOSE:
;
;  Calculate Lommel-Seeliger disk-function
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  d = lommel-seeliger(inc, emi)
;
; INPUTS:
;
;  inc        - Incidence angle in degrees
;  emi        - Emission angle in degrees
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the Lommel-Seeliger disk-function mu_0/(mu_0+mu)
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
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 15, 2017, created by JYL @PSI
;-

FUNCTION lommel_seeliger, inc, emi
    u0 = cos(inc * !dtor)
    u = cos(emi * !dtor)
    RETURN, u0/(u+u0)
END
