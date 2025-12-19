;+
; NAME:
;
;  boloalbedos.pro
;
; PURPOSE:
;
;  Calculate bolometric Bond albedo for OVIRS data
;
; CATEGORY:
;
;  OVIRS photometric processing
;
; CALLING SEQUENCE:
;
;  boloalbedos[, infile][, outfile][, parfile][, model=model][, solar=solar]
;             [, configuration=conf][, /quiet][, /help][, /manual]
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;  infile     - Input data file name
;  outfile    - Output data file name
;  parfile    - Photometric parameter file name
;
; KEYWORD PARAMETERS:
;
;  model      - Model name used for photometric correction.  The
;               options are 'lommel-seeliger', 'rolo', 'minnaert', mcewen'
;               (case insensitive).  If the specified model is not recognized,
;               then program will generate an error.
;  solar      - File name for solar spectrum.  Default is 'solar_flux.txt' in
;               the same directory as this program
;  configuration - Configuration file name.  Default is 'PhotCorrS.conf' in
;               the same directory as this program file.  If configuration
;               file not found, program will generate an error.
;  quiet      - Quiet mode
;  help       - Display usage help
;  manual     - Display software manual
;
; OUTPUTS:
;
;  None
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
; PROCEDURE USED:
;
;  findpro.pro, which in turn uses 'fdecomp.pro' both in Goddard IDL
;  Astronomy User's Library (http://idlastro.gsfc.nasa.gov)
;  load_conf.pro, read_spec.pro, read_phomodel.pro phot_model.pro,
;  write_spec.pro
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : July 14, 2016, created by JYL @PSI
;  v1.1 : July 21, 2016, modified by JYL @PSI
;     Propagate configuration parameters out to output file
;  v1.2 : Jan 12, 2017, modified by JYL @PSI
;    * Change file name and routine name to small case
;  v1.2.1 : March 16, 2017, JYL @PSI
;    * Updated the calculation of phase integral and bolometric albedo with
;      quality flag
;  March 27, 2017, JYL @PSI
;    Use system variable !quiet and `message` to control info message
;    Change keyword `verbose` to `quiet`
;  March 28, 2017, JYL @PSI
;    Add keyword `status` for simple error handling
;  March 29, 2017, JYL @PSI
;    Add traceback info to error handler printout
;  April 7, 2017, JYL @PSI
;    Use `print` rather than `message` to print info messages
;-

PRO boloalbedos, infile, outfile, parfile, model=model, solar=solar, configuration=conf, quiet=quiet, help=hp, manual=man, status=status, benchmark=bench, _Extra=ex

    version = '3.0'
    intvrsnb = '20180206'

    IF keyword_set(hp) THEN BEGIN
        message, /info,/noname, "Usage: boloalbedos[, 'infile'][, 'outfile'][, 'parfile']"
        message, /info,/noname, "                  [, model='model'][, solar='solar'][, /quiet]"
        message, /info,/noname, "Or:    boloalbedos[, configuration='configuration_file']"
        message, /info,/noname, "Or:    boloalbedos, /manual"
        status = 1
        RETURN
    ENDIF

    ; display help information
    findpro, 'photcorrs', /noprint, dirlist=d
    d = d[0]
    IF keyword_set(man) THEN BEGIN
        xdisplayfile, d+'manual.txt'
        status = 2
        RETURN
    ENDIF

    ; establish error handler
    catch, error_code
    IF error_code NE 0 THEN BEGIN
        help, /last_message, output=str
        FOR i=0, n_elements(str)-1 DO message, /info, /noname, /noprefix, str[i]
        status = -1
        RETURN
    ENDIF

    ; process input and check input files
    input = {}
    IF n_params() GE 1 THEN input = create_struct(input, 'infile', infile)
    IF n_params() GE 2 THEN input = create_struct(input, 'outfile', outfile)
    IF n_params() EQ 3 THEN input = create_struct(input, 'parfile', parfile)
    IF keyword_set(model) THEN input = create_struct(input, 'model', model)
    IF keyword_set(solar) THEN input = create_struct(input, 'solar', solar)
    IF size(quiet,/type) NE 0 THEN input = create_struct(input, 'quiet', quiet)
    IF keyword_set(bench) THEN input = create_struct(input, 'benchmark', bench)
    IF NOT keyword_set(conf) THEN conf = d+'boloalbedos.conf'
    IF ~file_test(conf) THEN message, 'configuration file '+conf+' not found'
    con = load_conf(conf, input=input)

    IF ~file_test(con.parfile) THEN message, 'parameter file '+con.parfile+' not found'
    IF strlowcase(con.model) EQ 'best' THEN BEGIN
        dummy = read_phomodel(con.parfile, 'lommel-seeliger', sis=con.phosis, header=parhdr, quiet=con.quiet)
        con.model = strtrim(sxpar(parhdr, 'mnbest'),2)
    ENDIF

    IF ~con.quiet THEN BEGIN
        print
        print, systime()
        print, 'boloalbedos '+version
        print, 'Input file: '+con.infile
        print, 'Output file: '+con.outfile
        print, 'Photometric parameter file: '+con.parfile
        print, 'Model: '+con.model
        print, 'Solar flux file: '+con.solar
        print, 'Configuration file: '+conf
        print
    ENDIF

    IF con.benchmark THEN t0 = systime(1)

    ; read the data to be corrected
    iof_data = read_spec(con.infile, error=iof_err_data, wav=wav, info=info, header=iofhdr, quiet=con.quiet)
    ; read photometric model parameters
    p = read_phomodel(con.parfile, con.model, header=parhdr, sis=con.phosis, wav=wav, covar=cov, quiet=con.quiet)
    IF strtrim(sxpar(parhdr, 'FITTED')) EQ 'NO' THEN message, 'Model '+con.model+' is not fitted!'
    sz = size(iof_data,/dimension)
    nwv = sz[0]
    nsp = sz[1]
    IF con.benchmark THEN t1 = benchmark(t0, 'Load data')

    ; prepare output variables
    bba = fltarr(nsp)
    bba_err = fltarr(nsp)

    ; only work on valid data and geometry
    good = where((info.incidang LT 90) AND (info.emissang LT 90) AND  $
        total(abs([[info.incidang], [info.emissang], [info.phaseang]]),2) GT 0)
    IF good[0] NE -1 THEN BEGIN
        ngood = n_elements(good)
;;;;;;;;;;;;;;;;
        nwv = 1224
;;;;;;;;;;;;;;;;        
        ; calculate phase integral
        qs = fltarr(nwv)
        qe = fltarr(nwv)
        flags = uintarr(nwv)
        ; loop through valid parameters
        FOR i=0, nwv-1 DO BEGIN
            qs[i] = phase_integral(con.model, p[i,*],  $
                    perr=reform(cov[i,*,*]), err=err, flag=flag)
            qe[i] = err
            flags[i] = flag
        ENDFOR
        IF con.benchmark THEN t2 = benchmark(t1, 'Phase integral')

        ; calculate normal albedo
        iof_corr = fltarr(nwv,ngood)
        corr_err = fltarr(nwv,ngood)
        FOR i=0, nwv-1 DO BEGIN
            err = iof_err_data[i,good]
            iof_corr[i,*] = photomet(con.model, p[i,*], iof_data[i,good],  $
              info.phaseang[good], info.incidang[good], info.emissang[good],  $
              refpha=0., refinc=0., refemi=0., err=err, /radf)
            corr_err[i,*] = err
        ENDFOR
        IF con.benchmark THEN t3 = benchmark(t2, 'Normal albedo')

        ; Bond albedo by wavelength
        bond = fltarr(nwv,ngood)
        qss = qs # replicate(1, ngood)
        bond = iof_corr * qss
        ; Bond albedo errors
        bond_err = fltarr(sz)
        q_relerr = fltarr(size(qe,/dim))
        ww = where(qs GT 0)
        IF ww[0] NE -1 THEN q_relerr[ww] = qe[ww]/qs[ww]
        iof_relerr = fltarr(nwv,ngood)
        ww = where(iof_corr EQ 0)
        IF ww[0] NE -1 THEN iof_corr[ww] = 1.
        iof_relerr = corr_err / iof_corr
        IF ww[0] NE -1 THEN iof_relerr[ww] = 0.
        bond_err = (q_relerr # replicate(1, ngood) + iof_relerr) * bond
        IF con.benchmark THEN t4 = benchmark(t3, 'Bond albedo')

        ; bolometric Bond albedo
        read_solar, con.solar, swv, sflx, quiet=con.quiet
        solar = [[swv], [sflx]]
        bba[good] = bolometric(bond, wav, solar=solar,  $
                               error=bond_err, boloerr=boloerr)
        bba_err[good] = boloerr
        
        IF con.benchmark THEN t5 = benchmark(t4, 'Bolometric Bond albedo')
        ;stop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; wav_cut = wav[0:1224]
        ;berr = boloerr
        ;my2plot = plot(wav, boloerr,' g-', TITLE="",layout=[3,2,3], /current)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ENDIF

    ; prepare parameters to be propagated to output file
    prop = {}
    keys = ['atltgtid', 'sciseqid', 'seqdesc', 'obsstart', 'obsend', 'sclkstrt', 'sclkend']
    FOR i=0, n_elements(keys)-1 DO prop = create_struct(prop, keys[i], sxpar(iofhdr, keys[i]))
    prop = create_struct(prop, 'in_file', file_basename(con.infile))
    prop = create_struct(prop, 'mnbest', con.model)
    prop = create_struct(prop, 'phtmodfl', file_basename(con.parfile))
    prop = create_struct(prop, 'solrspec', con.solar)
    prop = create_struct(prop, 'ba_sw', 'boloalbedos.pro')
    prop = create_struct(prop, 'ba_ver', version)
    prop = create_struct(prop, 'idl_ver', !version.release)
    prop = create_struct(prop, 'os_name', !version.os_name)
    prop = create_struct(prop, 'sysarch', !version.arch)
    prop = create_struct(prop, 'intvrsnb', intvrsnb)

    ; save output spectral data
    write_bolo, con.outfile, bba, flags, info, error=bba_err, prop=prop, sis=con.bolosis, overwrite=con.overwrite, quiet=con.quiet

    IF con.benchmark THEN t6 = benchmark(t0, 'All steps')
    status = 0

END
