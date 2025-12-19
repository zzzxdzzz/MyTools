;+
; NAME:
;
;  bin_phodata.pro
;
; PURPOSE:
;
;  Bin photometric data in scattering geometry parameter space
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  bin_phodata, iof, pha, inc, emi, iofbin, phabin, incbin, emibin
;               [, ioferr][, phaerr][, incerr][, emierr]
;               [, phares=phares][, incres=incres][, emires=emires]
;               [, error=error]
;               [, count=count][, /quiet]
;
; INPUTS:
;
;  iof, pha, inc, emi - Photometric data arrays.  `iof' array can be multiband.
;     in this case, `iof' array is an (N x M) array, where N is the number of
;     bands, and M is the number of data points in each band, which is the
;     same as the number of elements in pha/inc/emi
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  phares, incres, emires - Resolution (bin size) in geometry space.  Defaults
;     are 5 deg.
;  error  - Uncertainty in I/F data
;  count  - Returns an array of the same size as the binned geometries,
;           containing the count of data points binned into each bin.
;  quiet  - If set, then all information messages are supppressed.  Default
;           is to be controlled by system variable !quiet.
;
; OUTPUTS:
;
;  iofbin, phabin, incbin, emibin - Binned photometric data arrays
;
; OPTIONAL OUTPUTS:
;
;  ioferr, phaerr, incerr, emierr - Error arrays of binned photometric data
;        IF the uncertainty of input data is not supplied, THEN `ioferr'
;        returns the standard deviation IF the corresponding `count'>=2, and 0
;        IF `count'=1.  IF the uncertainty of input data is supplied, THEN it
;        returns the error as propagated to the average.  `phaerr', `incerr',
;        and `emierr' all return the standard deviation of binned points IF
;        `count'>=2, or 0 otherwise.
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
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : Feb 2 2017, created by JYL @PSI
;    Based on bin_iof.pro in JYL's personal library
;  v1.0.1 : March 27, 2017, JYL @PSI
;    Use `message` and !quiet to control information messages
;    Change keyword `verbose` to `quiet`
;  April 7, 2017, JYL @PSI
;    Use `print` rather than `message` to print out information messages
;  Aug 7, 2017, XDZ @PSI
;    Modify ioferr calculation in one band situation.

;-

PRO bin_phodata, iof, pha, inc, emi, iofbin, phabin, incbin, emibin, ioferr, phaerr, incerr, emierr, phares=phares, incres=incres, emires=emires, error=error, count=count, quiet=quiet
    IF n_params() LT 8 THEN BEGIN
        message, /info, 'bin_phodata, iof, pha, inc, emi, iofbin, phabin, incbin, emibin[, ioferr][, phaerr][, incerr][, emierr][, phares=phares][, incres=incres][, emires=emires][, error=error][, count=count][, /quiet]'
        RETURN
    ENDIF

    IF NOT keyword_set(phares) THEN phares=5.
    IF NOT keyword_set(incres) THEN incres=5.
    IF NOT keyword_set(emires) THEN emires=5.
    IF keyword_set(error) THEN haserror=1 ELSE haserror=0
    IF NOT keyword_set(quiet) THEN quiet = !quiet

    maxinc = float(max(inc))
    mininc = float(min(inc))
    maxemi = float(max(emi))
    minemi = float(min(emi))
    maxpha = float(max(pha))
    minpha = float(min(pha))
    nbins = ceil((maxinc-mininc)/float(incres))*ceil((maxemi-minemi)/float(emires))*ceil((maxpha-minpha)/float(incres))+100

    IF ~quiet THEN BEGIN
        print
        print, 'Bin photometric data to grid:'
        print, '  i: from'+string(mininc,format='(f5.1)')+' to '+string(maxinc,format='(f5.1)')+' with bin size '+string(incres,format='(f4.1)')
        print, '  e: from'+string(mininc,format='(f5.1)')+' to '+string(maxemi,format='(f5.1)')+' with bin size '+string(emires,format='(f4.1)')
        print, '  a: from'+string(minpha,format='(f5.1)')+' to '+string(maxpha,format='(f5.1)')+' with bin size '+string(phares,format='(f5.1)')
    ENDIF

    IF size(iof,/n_dim) EQ 1 THEN nbd=1 else nbd = (size(iof,/dimensions))[0]

    incbin = (incerr = (emibin = (emierr = (phabin = (phaerr = fltarr(nbins))))))
    count = intarr(nbins)
    IF nbd GT 1 THEN iofbin = (ioferr = fltarr(nbd,nbins))  $
    else iofbin=(ioferr=fltarr(nbins))

    k = 0l
    FOR a=minpha, maxpha, phares DO BEGIN
        phaindx = where((pha GE a) AND (pha LT a+phares))
        IF phaindx[0] NE -1 THEN BEGIN
            FOR i=mininc, maxinc, incres DO BEGIN
                incindx = where((inc[phaindx] GE i) AND (inc[phaindx] LT i+incres))
                IF incindx[0] ne -1 THEN BEGIN
                    FOR e=minemi, maxemi, emires DO BEGIN
                        emiindx = where((emi[phaindx[incindx]] ge e) and (emi[phaindx[  incindx]] LT e+emires))
                        IF emiindx[0] ne -1 THEN BEGIN
                            binindx = phaindx[incindx[emiindx]]  ; these are indices of data in this bin
                            count[k] = n_elements(binindx)
                            incbin[k] = mean(inc[binindx])
                            incerr[k] = stddev(inc[binindx])
                            emibin[k] = mean(emi[binindx])
                            incerr[k] = stddev(emi[binindx])
                            phabin[k] = mean(pha[binindx])
                            phaerr[k] = stddev(pha[binindx])
                            IF nbd EQ 1 THEN BEGIN  ; one band
                                iofbin[k] = mean(iof[binindx])
                                IF haserror THEN ioferr[k] = sqrt(total(error[binindx]^2))/n_elements(binindx)  $
                                ELSE ioferr[k] = !values.f_nan;stddev(error[binindx])
                            ENDIF ELSE BEGIN  ; multiband
                                IF n_elements(binindx) EQ 1 THEN BEGIN
                                    iofbin[*,k] = iof[*,binindx]
                                    ;stop
                                    IF haserror THEN ioferr[*,k] = error[*,binindx] $
                                  ELSE ioferr[*,k] = !values.f_nan
                                ENDIF ELSE BEGIN
                                    iofbin[*,k] = mean(iof[*,binindx], dimension=2)
                                    IF haserror THEN BEGIN
                                        ioferr[*,k] = sqrt(total(error[*,binindx]^2, 2))/n_elements(binindx)
                                    ENDIF ELSE ioferr[*,k] = stddev(iof[*,binindx], dimension=2)
                                ENDELSE
                            ENDELSE
                            IF ~quiet THEN BEGIN
                                print, '  Non-empty bin:'
                                print, '    pha = ['+string(a,format='(f5.1)')+','+string(a+phares,format='(f5.1)')+')'
                                print, '    inc = ['+string(i,format='(f5.1)')+','+string(i+incres,format='(f5.1)')+')'
                                print, '    emi = ['+string(e,format='(f5.1)')+','+string(e+emires,format='(f5.1)')+')'
                                print, '    count = : '+string(count[k],format='(i3)')
                            ENDIF
                            k = k+1
                        ENDIF
                    ENDFOR
                ENDIF
            ENDFOR
        ENDIF
    ENDFOR
    count = count[0:k-1]
    incbin = incbin[0:k-1]
    emibin = emibin[0:k-1]
    phabin = phabin[0:k-1]
    iofbin = iofbin[*,0:k-1]
    incerr = incerr[0:k-1]
    emierr = emierr[0:k-1]
    phaerr = phaerr[0:k-1]
    ioferr = ioferr[*,0:k-1]

    IF ~quiet THEN BEGIN
        print, 'Binning complete.  Number of data points: '+string(k,format='(i6)')
        print
    ENDIF

END
