;+
; NAME:
;
;  plot_model_quality.pro
;
; PURPOSE:
;
;  Make model quality plot
;
; CATEGORY:
;
;  Plotting
;
; CALLING SEQUENCE:
;
;  plot_model_quality, 'inputfile_qua.fits'[, phopar=...]
;
; INPUTS:
;
;  inputfile_qua.fits - input file that contains the output quality data
;
; OPTIONAL INPUTS:
;
;  phopar  - Photometric parameters.  If provided, then geometric albedo will
;            be calculated and plotted
;
; KEYWORD PARAMETERS:
;
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
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
; PROCEDURE USED:
;
;   rdfits_struct.pro, mrdfits.pro, sxpar.pro
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0.0 : March 13, 2017, created by JYL @PSI
;  March 27, 2017, JYL @PSI
;    Use `message` to print information
;-

PRO plot_model_quality, quafits, phopar=phopar

    IF n_params() EQ 0 THEN BEGIN
        message, /info, 'Usage: plot_model_quality, quality_file.fits'
        message, /info, '    quality_file.fits : input quality file'
        RETURN
    ENDIF

    rdfits_struct, quafits, s
    tags = tag_names(s)
    n_mod = (n_elements(tags)-2)/2
    models = strarr(n_mod)

    outfile = strmid(quafits, 0, strpos(quafits, '.', /reverse_search))+'.ps'

    ss = {}
    FOR i=0, n_mod-1 DO BEGIN
        qua = mrdfits(quafits, i+1, hdr)
        models[i] = strtrim(sxpar(hdr, 'EXTNAME'),2)
        ss = create_struct(ss, 'ext'+string(i,format='(i1)'), qua)
    ENDFOR

    ; backup current settings
    devpar = !d
    ppar = !p
    xpar = !x
    ypar = !y

    ; open ps file and set up plot
    set_plot, 'ps'
    device, get_decomposed=decomp
    device, file=outfile, bits_per_pixel=8, /inches, xsize=7, ysize=9, yoffset=1, /portrait, /color, decomposed=0
    tvlct, red, green, blue, /get
    loadct, 13, /silent
    !p.multi = [0, 1, 4]
    color = [1, 80, 150, 200, 250]
   
    
    ; first plot - slope
    jj = [1,3,5,7]
    ;yran = [[0,1.5], [-0.1,0.1],[-0.1,0.1],[-0.1,0.1]]
    yran = [[0.8,1], [-0.01,0.01],[-0.01,0.01],[-0.01,0.02]]
    ytitle = 'Slope '+['','Phase','Incidence','Emission']
    title = ['Slope','','','']
    FOR i=0, 3 DO BEGIN  ; four panels
        plot, [0],[0], /nodata, xran=[0.3,3.6], yran=yran[*,i], ytitle=ytitle[i], xthick=5,ythick=5,charthick=5,charsize=2,xtitle='Wavelength (um)',xstyle=1, title=title[i]
        FOR j=0, n_mod-1 DO BEGIN  ; loop through models
            oplot, ss.(j).wav, ss.(j).(jj[i]), thick=3, color=color[j]
        ENDFOR
    ENDFOR

    ; second plot - correlation
    jj = [2,4,6,8]
    ;yran = [[0,1.1], [-1,1],[-1,1],[-1,1]]
    yran = [[0.7,1.05], [-0.22,0.22],[-0.6,0.6],[-0.6,0.9]]
    ytitle = 'Correlation '+['','Phase','Incidence','Emission']
    title = ['Correlation','','','']
    FOR i=0, 3 DO BEGIN  ; four panels
        plot, [0],[0], /nodata, xran=[0.3,3.6], yran=yran[*,i], ytitle=ytitle[i], xthick=5,ythick=5,charthick=5,charsize=2,xtitle='Wavelength (um)',xstyle=1, title=title[i]
        FOR j=0, n_mod-1 DO BEGIN  ; loop through models
            oplot, ss.(j).wav, ss.(j).(jj[i]), thick=3, color=color[j]
        ENDFOR
    ENDFOR

    ; third plot - geometric albedo
    IF keyword_set(phopar) THEN BEGIN
        !p.multi = 0
        nwv = n_elements(ss.(0).wav)
        pp = fltarr(n_mod, nwv)
        FOR i=0, n_mod-1 DO BEGIN  ; loop through models
            par = read_phomodel(phopar, models[i], wav=wv)
            for j=0, nwv-1 DO pp[i,j] = phot_model(models[i], 0,0,0, par[j,*], /radf)
        ENDFOR
        plot, [0],[0],/nodata,xran=[0.3,3.6],yran=[min(pp),max(pp)],xtitle='Wavelength (um)',ytitle='Normal Albedo',xthick=5,ythick=5,charthick=5,charsize=2
        FOR i=0,n_mod-1 DO oplot, wv,pp[i,*],color=color[i]
    ENDIF

    device, /close

    ; restore settings
    device, decomposed=decomp
    tvlct, red, green, blue
    set_plot, devpar.name
    !p = ppar
    !x = xpar
    !y = ypar

END

