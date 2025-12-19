;+
; NAME:
;
;  best_model.pro
;
; PURPOSE:
;
;  Determine the best model
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  best = best_model(quality_file)
;
; INPUTS:
;
;  quality_file : String, name of photometric model quality file
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the name of best model
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
; PROCEDURE:
;
;  Program calculate the means and standard deviations of four slopes and four
;  correlations along wavelength for all four models.  Then it uses the larger
;  one of the absolute values of mean and corresponding standard deviation to
;  calculates the sum of all eight quantities for each model.  This sum for
;  each model is used as a proxy to evaluate the relative quality of four
;  models, and the model with the smallest proxy is returned as the best model.
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;-

FUNCTION best_model, qfile

    fits_info, qfile, extname=models, /silent
    models = models[1:*]
    mnts = []
    FOR i=0, n_elements(models)-1 DO BEGIN
        q = mrdfits(qfile, models[i], /silent)
        nwv = n_elements(q)
        ; find good wavelengths
        ntags = n_elements(tag_names(q))
        good = fltarr(nwv)
        FOR j=0, nwv-1 DO FOR k=1,ntags-1 DO good[j]+=abs(q[j].(k))
        good = where(good GT 0)
        mnt = []
        FOR k=1, ntags-1 DO BEGIN
            mnt = [[mnt], [moment((q.(k))[good])]]
        ENDFOR
        mnts = [[[mnts]], [[mnt]]]
    ENDFOR

    mnts[0,0:1,*] -= 1
    qmarker = total(max(abs(mnts[0:1,*,*]),dimension=1),1)
    RETURN, models[(where(qmarker EQ min(qmarker)))[0]]
END
