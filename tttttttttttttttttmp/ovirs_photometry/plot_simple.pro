; NAME:
;
;   plot_simple
;
; PURPOSE:
;
;   After fitting, plot with given models and wavelenth
;
; Inputs:
;       infile:     input file for fitting, default is 'spdif.fits'
;       outfile:    output file from fitting, default is 'bennu_phopar.fits'
;       pltfile:    foldername
;       models:     select model to plot;(to be worked on)
;       wav:        selected wave length to plot
; Outputs:
;       /pltfile/modelname/plots
;
; MODIFICATION HISTORY:
;   v1.0 : Jan 10, 2017 created by XDZ @PSI
;   V1.1 : Mar 12, 2017 make changes to work with new version of
;photmods.pro by zxd
;   v1.2 : Mar 16, 2017 fix a bug for overplot
;-

PRO filled_circle_symbol
    aa=findgen(40)/39.*!pi*2.  ;set plots' symbols
    usersym,.5*cos(aa),.5*sin(aa),/fill ; set the size of the data points
END


PRO plot_simple, infile, outfile, plotfile, configuration=conf, model=model, wavmin=wavmin, wavmax=wavmax

    findpro, 'PhotModS', /noprint, dirlist=d
    d=d[0]

    ; Load configuration file
    input = {}
    IF n_params() GE 1 THEN input = create_struct(input, 'infile', infile)
    IF n_params() GE 2 THEN input = create_struct(input, 'outfile', outfile)
    IF n_params() EQ 3 THEN input = create_struct(input, 'plotfile', plotfile)
    IF keyword_set(model) THEN input = create_struct(input, 'model', model)
    IF NOT keyword_set(conf) THEN conf = d+'photmods.conf'
    IF ~file_test(conf) THEN message, 'configuration file '+conf+' not found'
    con = load_conf(conf,input=input)
    plotfile = con.plotfile

    ; load color table
    filled_circle_symbol

    ; load input, get iof, pha,inc,emi
    data = read_spec(con.infile, error=error, wav=wav, info=info, header=header, verbose=verbose)
    tmp = size(data, /dim)
    nwv = tmp[0]
    nsp = tmp[1]

    ; bin data in scattering parameter space
    IF con.phobin THEN  BEGIN
        bin_phodata, data, info.phaseang, info.incidang, info.emissang, iofb, phab, incb, emib, errb, phares=con.phabin, incres=con.incbin, emires=con.emibin, verbose=verb
    ENDIF ELSE BEGIN
        iofb = data
        phab = info.phaseang
        incb = info.incidang
        emib = info.emissang
        errb = error
    ENDELSE

    ; loop through model, use phot_model to get iofmod

    IF keyword_set(model) THEN models=[model] ELSE models=con.model


    FOR i=0, n_elements(models)-1 DO BEGIN

        ; read p from outfile, only read the spcific model
        ; Didn't work on specific model yet,only can read all model for now
        p_model = mrdfits(con.outfile, i)

        ; prepare for plot
        fdecomp, plotfile, disk, path, name
        path = path + models[i]
        IF file_test(path) EQ 0 THEN file_mkdir, path
        IF !version.os_family EQ 'Windows' THEN slash = '\' ELSE slash = '/'
        pltfile = path+slash+name+'_'+models[i]

        ; find wav index from input wave length limits
        IF keyword_set(wavmin) THEN BEGIN
            FOR k=0, nwv-1 DO BEGIN
                IF wav[k] GE wavmin THEN BEGIN
                    indexmin = k
                    break
                ENDIF
            ENDFOR
        ENDIF ELSE BEGIN
            indexmin = 0
        ENDELSE

        IF keyword_set(wavmax) THEN BEGIN
            FOR k=nwv-1, 0, -1 DO BEGIN
                IF wav[k] LE wavmax THEN BEGIN
                    indexmax = k
                    break
                ENDIF
            ENDFOR
        ENDIF ELSE BEGIN
            indexmax = nwv-1
        ENDELSE

        IF indexmax LE indexmin THEN message, 'Plot wave range wrong.'

        ; data pre-selection base on geometry
        geom = (incb GT con.incmin) AND $
               (incb LT con.incmax) AND $
               (emib GT con.emimin) AND $
               (emib LT con.emimax) AND $
               (phab GT con.phamin) AND $
               (phab LT con.phamax)

        ; loop through wave length
        FOR k=indexmin, indexmax DO BEGIN
            iof = iofb[k,*]
            in = where(geom AND (iof GT con.iofmin) AND (iof LT con.iofmax))
            status = -255

            IF in[0] NE -1 THEN BEGIN
                iof = iof[in]
                ioferr = (errb[k,in])[*]
                pha = phab[in]
                inc = incb[in]
                emi = emib[in]

                ; check the validity of data
                status = check_phodata(iof, pha, inc, emi, ioferr=ioferr, verbose=verb)
            ENDIF

            IF status GE 0 THEN BEGIN

                ; load p's data from given wavelength, remove wav
                p = transpose(p_model[k,2:*])

                iofmod = phot_model(models[i], pha, inc, emi, p, /radf)
                ; make plot
                plot_ps, pltfile, models[i], p, iof, iofmod, pha, inc, emi, con, wav=wav[k]
            ENDIF
        ENDFOR
    ENDFOR
END
