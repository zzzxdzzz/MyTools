;+
; NAME:
;
;  plot_ps.pro
;
; PURPOSE:
;
;  Plot model fitting results
;
; CATEGORY:
;
;  Plotting
;
; CALLING SEQUENCE:
;
;  plot_PS, plotfile, model, p, iof, iofmodel, pha, inc, emi, info[,
;  wav=wav]
;
; INPUTS:
;
;  plotfile   - A string of the path and root name of the output file.  Note
;               that a subdirectory will be created with the name of the model
;               to save all plot files, and the name of model and wavelength
;               (if necessary) will be added to `plotfile' string for plot file
;               names.
;  model      - A string to specify the photometric model.  Can be
;               'lommel-seeliger', 'rolo', 'minnaert', 'mcewen'.
;  iof        - I/F data
;  iofmodel   - Modeled I/F data
;  pha, inc, emi - Phase angle, incidence angle, and emission angle of the
;               I/F data, in degrees
;  info       - A structure containing the information of data selection range
;               The fields used by this program (must present) are: .incmin,
;               .incmax, .emimin, .emimax, .phamin, .phamax, .resmin,
;               .resmax.
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  wav        - Wavelength.  If supplied, it will be printed in the
;               plot
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
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
; PROCEDURE USED:
;
;   mrdfits.pro and all subroutines used by mrdfits.pro in Goddard IDL
;   Astronomy User's Library (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : May 25, 2016, created by JYL @PSI
;  v2.0 : Mar 16, 2017, add nowave parameter to fix plot by ZXD@psi
;  v2.1 : March 16, 2017, JYL@PSI
;    * modified the way to generate file name
;    * removed keyword nowav
;
;-

PRO plot_ps, plotfile, model, p, iof, iofmodel, pha, inc, emi, info, wav=wav

    ; save current plotting settings
    devpar = !d
    ppar = !p
    xpar = !x
    ypar = !y

    ; prepare plotting parameters
    !p.charsize=1.2
    !p.font=0
    !p.psym = 8
    !p.thick = 4
    !p.linestyle = 0
    !x.thick = 1.2
    !y.thick = 1.0
    !y.style = 1
    set_plot, 'ps'

    ; load color setting
    device, get_decomposed=decomp
    device, decomposed=0
    tvlct, red, green, blue, /get
    loadct, 39, /silent

    ; prepare data
    ratio = iofmodel/iof
    iofmean = mean(iof)
    iofres_total = total((iof-iofmean)^2)
    res = total((iof-iofmodel)^2)
    R_2 = 1-res/iofres_total
    c = linfit(iof, iofmodel, chisq=chisq_m)
    IF keyword_set(wav) THEN BEGIN
        wav1 = ' ('+string(fix(wav),format='(i4)')+' nm)'
        wav2 = '_'+string(fix(wav*1000),format='(i04)')
    ENDIF ELSE wav1 = (wav2 = '')

    ; make first plot i/f_modeled vs. i/f_measured
    ; plot parameters
    IF strcmp(model, 'lommel-seeliger', 5, /fold_case) EQ 1 THEN BEGIN
        xx = [0.02, 0.005, 0.005, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.01, 0.01, 0.01, 0.01, 0.01]
        yy = [0.25, 0.2, 0.185, 0.1, 0.085, 0.07, 0.055, 0.03, 0.01, 0.17, 0.155, 0.145, 0.135, 0.125]
        sz = [1.5, 1.2, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 0.9, 0.9, 0.9]
        strtitle='Lommel Seeliger fit to Bennu Spectra'+wav1
        str = [' ',$
               '(I/F) = {!9p!xA!DLS!Nf!DLS!N(!9a!x)[!9m!x!Do!N/(!9m!x!Do!N+!9m!x)]}', $
          'f!DLS!N(!9a!x)=exp(!9b!x!9a!x+!9g!x!9a!x!E2!N+!9d!x!9a!x!E3!N ', $
          'A!DLS!N='+strtrim(string(p[0], format='(d20.3)'),2), $
          '!9b!x ='+strtrim(string(p[1], format='(d20.4)'),2), $
          '!9g!x = ' +strtrim(string(p[2], format='(d20.6)'),2), $
          '!9d!x = ' +strtrim(string(p[3], format='(d20.9)'),2), $
          '!9c!x!E2!N=' +strtrim(string(chisq_m, format='(d20.2)'),2), $
          'R!E2!N=' +strtrim(string(R_2, format='(d20.2)'),2), $
          ' Data selection:', $
          strtrim(string(info.incmin),2)+'!Eo!N'+' < incidence angle (!16i!X) < ' + strtrim(string(info.incmax),2)+'!Eo!N', $
        strtrim(string(info.emimin),2)+'!Eo!N'+' < emission angle (!16i!X) < ' + strtrim(string(info.emimax),2)+'!Eo!N', $
        strtrim(string(info.phamin),2)+'!Eo!N'+' < phase angle(!9a!x) < ' + strtrim(string(info.phamax),2)+'!Eo!N', $
               strtrim(string(info.resmin, format='(F0.2)'),2)+'km'+' < spatial resolution < ' + strtrim(string(info.resmax, format='(F0.2)'),2)+'km']

    ENDIF ELSE IF strcmp(model, 'rolo', 5, /fold_case) EQ 1 THEN BEGIN
        xx = [0.04, 0.01, 0.01, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.01, 0.01, 0.01, 0.01, 0.01]
        yy = [0.22, 0.2, 0.19, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07, 0.06, 0.04, 0.02, 0.17, 0.155, 0.145, 0.135, 0.125]
        sz = [1.5, 1., 0.8, 1, 1, 1, 1, 1, 1, 1, 1 ,1, 1, 0.9, 0.9, 0.9, 0.9]
        strtitle='ROLO Fit to Bennu Spectra'+wav1
        str = [' ',$
               'I/F = {[f!DR!N(!9a!x)!9m!x!Do!N]/(!9m!x!Do!N+!9m!x)}', $
          'f!DR!N(!9a!x)= C!D0!Nexp-(C!D1!N!9a!x)+A!D0!N+A!D1!N!9a!x+A!D2!N!9a!x!E2!N+A!D3!N!9a!x!E3!N+A!D4!N!9a!x!E4!N', $
          'C!D0!N='+strtrim(string(p[0], format='(d20.3)'),2), $
          'C!D1!N ='+strtrim(string(p[1], format='(d20.4)'),2), $
          'A!D0!N = ' +strtrim(string(p[2], format='(d20.6)'),2), $
          'A!D1!N = ' +strtrim(string(p[3], format='(d20.9)'),2), $
          'A!D2!N = '+strtrim(string(p[4], format='(d20.12)'),2), $
          'A!D3!N = '+strtrim(string(p[5], format='(d20.12)'),2), $
          'A!D4!N = '+strtrim(string(p[6], format='(d20.12)'),2), $
          '!9c!x!E2!N=' +strtrim(string(chisq_m, format='(d20.2)'),2), $
          'R!E2!N=' +strtrim(string(R_2, format='(d20.2)'),2), $
          ' Data selection:', $
          strtrim(string(info.incmin),2)+'!Eo!N'+' < incidence angle (!16i!X) < ' + strtrim(string(info.incmax),2)+'!Eo!N', $
          strtrim(string(info.emimin),2)+'!Eo!N'+' < emission angle (!16i!X) < ' + strtrim(string(info.emimax),2)+'!Eo!N', $
          strtrim(string(info.phamin),2)+'!Eo!N'+' < phase angle(!9a!x) < ' + strtrim(string(info.phamax),2)+'!Eo!N', $
               strtrim(string(info.resmin, format='(F0.2)'),2)+'km'+' < spatial resolution < ' + strtrim(string(info.resmax, format='(F0.2)'),2)+'km']

    ENDIF ELSE IF strcmp(model, 'minnaert', 5, /fold_case) EQ 1 THEN BEGIN
        xx = [0.04, 0.007, 0.007, 0.007, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.01, 0.01, 0.01, 0.01, 0.01]
        yy = [0.27, 0.23, 0.215, 0.2, 0.12, 0.105, 0.09, 0.075, 0.06, 0.045, 0.025, 0.01, 0.17, 0.155, 0.145, 0.135, 0.125]
        sz = [1.5, 1.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 0.9, 0.9, 0.9]
        strtitle='Minnaert fit to Bennu Spectra'+wav1
        str = [' ',$
               '(I/F) = [!9p!xA!DM!Nf!DM!N(!9a!x)(!9m!x!Do!N!9m!x)!Ek(!9a!x)!N]/!9m!x', $
          'f!DM!N(!9a!x)=10^-[(!9b!x!9a!x + !9g!x!9a!x!E2!N +!9d!x!9a!x!E3!N)/2.5]', $
          'k(!9a!x)= k!Do!N+b!9a!x', $
          'A!DM!N='+strtrim(string(p[0], format='(d20.3)'),2), $
          '!9b!x ='+strtrim(string(p[1], format='(d20.4)'),2), $
          '!9g!x = ' +strtrim(string(p[2], format='(d20.6)'),2), $
          '!9d!x = ' +strtrim(string(p[3], format='(d20.9)'),2), $
          'k!Do!N = '+strtrim(string(p[4], format='(d20.3)'),2), $
          'b = '+strtrim(string(p[5], format='(d20.4)'),2), $
          '!9c!x!E2!N=' +strtrim(string(chisq_m, format='(d20.2)'),2), $
          'R!E2!N=' +strtrim(string(R_2, format='(d20.2)'),2), $
          ' Data selection:', $
          strtrim(string(info.incmin),2)+'!Eo!N'+' < incidence angle (!16i!X) < ' + strtrim(string(info.incmax),2)+'!Eo!N', $
          strtrim(string(info.emimin),2)+'!Eo!N'+' < emission angle (!16i!X) < ' + strtrim(string(info.emimax),2)+'!Eo!N', $
          strtrim(string(info.phamin),2)+'!Eo!N'+' < phase angle(!9a!x) < ' + strtrim(string(info.phamax),2)+'!Eo!N', $
               strtrim(string(info.resmin, format='(F0.2)'),2)+'km'+' < spatial resolution < ' + strtrim(string(info.resmax, format='(F0.2)'),2)+'km']

    ENDIF ELSE IF strcmp(model, 'mcewen', 5, /fold_case) EQ 1 THEN BEGIN
        xx = [0.02, 0.005, 0.005, 0.23, 0.23, 0.23, 0.23, 0.23, 0.23, 0.01, 0.01, 0.01, 0.01, 0.01]
        yy = [0.27, 0.25, 0.23, 0.12, 0.105, 0.09, 0.075, 0.06, 0.045, 0.17, 0.155, 0.145, 0.135, 0.125]
        sz = [1.5, 1.2, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 0.9, 0.9, 0.9]
        strtitle='McEwen Fit to Bennu Images'+wav1
        str = [' ',$
               '(I/F) = !9p!xA!DMC!N([2L(!9a!x)!9m!x!Do!N/(!9m!x!Do!N+!9m!x)] + [1 - L(!9a!x)]!9m!x!Do!N)', $
          'L!DMC!N(!9a!x)=exp(!9b!x!9a!x+!9g!x!9a!x!E2!N+!9d!x!9a!x!E3!N)', $
          'A!DMC!N='+strtrim(string(p[0], format='(d20.3)'),2), $
          '!9b!x ='+strtrim(string(p[1], format='(d20.4)'),2), $
          '!9g!x = ' +strtrim(string(p[2], format='(d20.6)'),2), $
          '!9d!x = ' +strtrim(string(p[3], format='(d20.9)'),2), $
          '!9c!x!E2!N=' +strtrim(string(chisq_m, format='(d20.2)'),2), $
          'R!E2!N=' +strtrim(string(R_2, format='(d20.2)'),2), $
          ' Data selection:', $
          strtrim(string(info.incmin),2)+'!Eo!N'+' < incidence angle (!16i!X) < ' + strtrim(string(info.incmax),2)+'!Eo!N', $
          strtrim(string(info.emimin),2)+'!Eo!N'+' < emission angle (!16i!X) < ' + strtrim(string(info.emimax),2)+'!Eo!N', $
          strtrim(string(info.phamin),2)+'!Eo!N'+' < phase angle(!9a!x) < ' + strtrim(string(info.phamax),2)+'!Eo!N', $
          strtrim(string(info.resmin, format='(F0.2)'),2)+'km'+' < spatial resolution < ' + strtrim(string(info.resmax, format='(F0.2)'),2)+'km']
    ENDIF

    ; generate plot
    plotfile1 = plotfile+'_Fit'+wav2+'.ps'
    device,/schoolbook, file=plotfile1,/color,xs=20,ys=25,xoff=1,yoff=1, decompose=0
    !p.multi=[0,1,2]
    range = [min([iof, iofmodel]), max([iof, iofmodel])]
    plot, [0], [0], /nodata,xra=range,xminor =.5 ,xst=.05,yra=range,ytit='(I/F)!DModeled!N', charsize = 1, xtit='(I/F)!DMeasured!N', yminor=.5, title=strtitle
    oplot,iof, iofmodel, col=250
    oplot, range, range, col = 60, ps=-1
    ; labels
    FOR i=0, n_elements(xx)-1 DO xyouts, xx[i], yy[i], str[i], size=sz[i], col=0
    device, /close

    ; second plot ratio vs. geometry
    plotfile2 = plotfile+'_Viewing_Geometry'+wav2+'.ps'
    device,fil=plotfile2,/color,xs=20,ys=25,xoff=1,yoff=1, decompose=0
    !p.multi = [0,1,3]
    ; ratio vs. phase angle
    plot,[0], [0], /nodata,xra=[0 ,120],xminor = 2 ,xst=.1,yra=[.5,1.5],ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Phase Angle(!16!9a!x'+'!Eo!N)',yminor=1
    oplot,pha, ratio, col=250
    oplot, [0, 120], [1, 1], col = 60, ps=-1
    xyouts, 35, 1.4, 'Fit vs. Phase Angle', siz=1.5, col =0; , /data
    ; ratio vs. incidence angle
    plot,[0],[0],/nodata,xra=[0 ,80],xminor = 2 ,xst=.1,yra=[.5,1.5],ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Incidence Angle(!16i!X!Eo!N)', yminor=1
    oplot,inc, ratio, col=250
    oplot, [0, 80], [1, 1], col = 60, ps=-1
    xyouts, 18, 1.4, 'Fit vs. Incidence Angle', siz=1.5, col = 0; , /data
    ; ratio vs. emission angle
    plot,[0],[0],/nodata,xra=[0 ,80],xminor = 2 ,xst=.1,yra=[.5,1.5],ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Emission Angle(!16e!X!Eo!N)',yminor=1
    oplot,emi, ratio, col=250
    oplot, [0, 80], [1, 1], col = 60, ps=-1
    xyouts, 19, 1.4, 'Fit vs. Emission Angle', siz=1.5, col =0;, /data
    device, /close

    ; restore color settings
    tvlct, red, green, blue
    device, decomposed=decomp

    ; restore plotting settings
    set_plot, devpar.name
    !p.charsize = ppar.charsize
    !p.font = ppar.font
    !p.multi = ppar.multi
    !p.psym = ppar.psym
    !p.thick = ppar.thick
    !p.linestyle = ppar.linestyle
    !x.thick = xpar.thick
    !y.thick = ypar.thick

END
