;+
; NAME:
;
;  iea_sphere.pro
;
; PURPOSE:
;
;  Calculate the incidence, emission, and phase angle of a sphere at specified
;  illumination and observing geometry
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  iea_sphere, pha, imap, emap, mask, nstep=nstep
;
; INPUTS:
;
;  pha    - Phase angle in degrees
;
; OPTIONAL INPUTS:
;
;  nstep  - Number of steps across the diameter of the sphere
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  imap, emap  - Named variables returning the 2-D maps of incidence and
;                emission angles
;  mask        - Named variable returning a mask of the same size as `imap` and
;                `emap`, with 2 means illuminated and visible, 1 means
;                unilluminated but visible, and 0 means outside of the sphere
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
;   9/21/2018, created by JYL @PSI
;-

PRO iea_sphere, pha, imap, emap, mask, nstep=nstep

    IF NOT keyword_set(nstep) THEN nstep=300
    x = findgen(nstep)/(nstep-1)*2-1
    xarr = x # replicate(1, nstep)
    xarr2 = xarr*xarr
    yarr = replicate(1, nstep) # x
    yarr2 = yarr*yarr
    darr2 = xarr2+yarr2
    ww = where(darr2 LT 1)
    zarr = fltarr(nstep, nstep)
    emap = fltarr(nstep, nstep)
    imap = fltarr(nstep, nstep)
    mask = intarr(nstep, nstep)
    IF ww[0] NE -1 THEN BEGIN
        zarr[ww] = sqrt(1-darr2[ww])
        emap[ww] = asin(sqrt(darr2[ww])) / !dtor
        vnorm = [[xarr[ww]],[yarr[ww]],[zarr[ww]]]
        vsun = replicate(1, (size(vnorm,/dim))[0]) # [sin(pha*!dtor), 0, cos(pha*!dtor)]
        imap[ww] = acos(total(vnorm*vsun, 2)) / !dtor
        mask[ww] = 1
        ww1 = where(imap[ww] LT 90 AND emap[ww] LT 90)
        if ww1[0] NE -1 THEN mask[ww[ww1]] = 2
    ENDIF

END
