;+
; NAME:
;
;  diskint.pro
;
; PURPOSE:
;
;  Calculate integrated disk-function numerically
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  Phi = diskint(model, pha[, p1[, p2[, p3]]][, nstep=200])
;
; INPUTS:
;
;  model  - A string specifies the name of model, can be 'lommel-seeliger',
;           'rolo', 'minnaert', 'mcewen', and 'akimov'
;  pha    - Phase angle in degrees
;  p1, p2, p3  - Model parameters if needed.
;                Minnaert model: k = p1, or k = p1+p2*phase
;                McEwen model: p1, p2, p3
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  nstep  - Number of steps along the diameter of sphere in the calculation
;           of integration.  For default of nstep=200, it takes about 0.7 s to
;           calculate a phase function from 0 to 180 deg with 1 deg step.
;           Computational time increase slightly slower than nstep^2.
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
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;  9/21/2018, created by JYL @PSI
;-

FUNCTION diskint, model, pha, p1, p2, p3, nstep=nstep

    IF NOT keyword_set(nstep) THEN nstep=200
    flx = fltarr(n_elements(pha))
    IF (strcmp(model, 'lommel-seeliger', 5, /fold_case) EQ 1) OR  $
       (strcmp(model, 'rolo', 5, /fold_case) EQ 1) THEN  $
    FOR i=0, n_elements(pha)-1 DO BEGIN
        iea_sphere, pha[i], imap, emap, mask, nstep=nstep
        img = imap*0
        ww = where(mask EQ 2)
        IF ww[0] NE -1 THEN img[ww] = lommel_seeliger(imap[ww], emap[ww])
        flx[i] = total(img)
    ENDFOR ELSE IF strcmp(model, 'minnaert', 5, /fold_case) EQ 1 THEN  $
    FOR i=0, n_elements(pha)-1 DO BEGIN
        iea_sphere, pha[i], imap, emap, mask, nstep=nstep
        img = imap*0
        ww = where(mask EQ 2)
        IF (size(p2))[0] EQ 0 THEN k = p1 ELSE k = poly(pha[i], [p1, p2])
        IF ww[0] NE -1 THEN img[ww] = minnaert(imap[ww], emap[ww], k)
        flx[i] = total(img)
        if ~finite(flx[i]) then stop
    ENDFOR ELSE IF strcmp(model, 'mcewen', 5, /fold_case) EQ 1 THEN  $
    FOR i=0, n_elements(pha)-1 DO BEGIN
        iea_sphere, pha[i], imap, emap, mask, nstep=nstep
        img = imap*0
        ww = where(mask EQ 2)
        phaww = replicate(pha[i], n_elements(ww))
        IF ww[0] NE -1 THEN img[ww] = mcewen(phaww, imap[ww], emap[ww], p1, p2, p3)
        flx[i] = total(img)
    ENDFOR ELSE IF strcmp(model, 'akimov', 5, /fold_case) EQ 1 THEN  $
    FOR i=0, n_elements(pha)-1 DO BEGIN
            iea_sphere, pha[i], imap, emap, mask, nstep=nstep
            img = imap*0
            ww = where(mask EQ 2)
            phaww = replicate(pha[i], n_elements(ww))
            IF ww[0] NE -1 THEN img[ww] = akimov(imap[ww], emap[ww], replicate(pha[i],n_elements(ww)))
            flx[i] = total(img)
    ENDFOR ELSE message, 'model not implemented: '+model

    RETURN, flx/max(flx)

END
