;+
; NAME:
;
;  photcorrs.pro
;
; PURPOSE:
;
;  Apply photometric correction to OVIRS spectral data
;
; CATEGORY:
;
;  OVIRS photometric processing
;
; CALLING SEQUENCE:
;
;  photcorrs[, infile][, outfile][, parfile][, model=model][, configuration=conf][, /quiet][, /help][, /manual]
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
;  configuration - Configuration file name.  Default is 'photcorrs.conf' in
;               the same directory as this program file.  If configuration
;               file not found, program will generate an error.
;  model      - Model name used for photometric correction.  The
;               options are 'lommel-seeliger', 'rolo', 'minnaert', mcewen'
;               (case insensitive).  If the specified model is not recognized,
;               then program will generate an error.
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
;  v1.0 : June 7, 2016, created by JYL @PSI
;
;  v1.1 : July 12,2016, modified by XDZ @PSI
;  * Corrected a bug can cause configuration file couldn't be found in
;      linux
;
;  v1.2 : July 20, 2016, modified by JYL @PSI
;    * Added the capability to input and output any type of reflectance
;      quantity, including bidirectional reflectance, RADF, REFF, and BRDF.
;      The types of I/O quantities are set in the configuration file under keys
;      `in' and `out'.
;    * Corrected a bug that caused the data from only the first two wavelength
;      chanel to be photometrically corrected.
;  v1.3 : Dec 22,2016, modified by XDZ @PSI
;    * Changed the tag names of the structure 'info'
;    * Fixed a bug when some bands doesn't have fitting results
;  v1.4 : Jan 6, 2017, modified by JYL @PSI
;    * Improved the checking for input photometric parameters.  In particular,
;      if all photoemtric parameters are 0, then skip correction and fill the
;      output with zeros.
;    * Fix bug in photometric correction
;  v1.5 : Jan 12, 2017, modified by JYL @PSI
;    * Change file name and routine name to small case
;  March 27, 2017, JYL @PSI
;    Use system variable !quiet and `message` to control info message
;    Change keyword `verbose` to `quiet`
;  March 28, 2017, JYL @PSI
;    Add keyword `status` for simple error handling
;  March 29, 2017, JYL @PSI
;    Add traceback info to error handler printout
;  April 7, 2017, JYL @PSI
;    Use `print` rather than `message` to print out info messages
;  Jun 28, 2017, XDZ @PSI
;    Change parameter reading process, read parameter covariance matrix
;    Add corr error calculation base on parameter covariance
;-

PRO photcorrs, infile, outfile, parfile, model=model, configuration=conf, quiet=quiet, help=hp, manual=man, status=status, benchmark=bench, _Extra=ex

    version = '3.0'
    intvrsnb = '20180206'  ; internal version number

    ; print short usage info
    IF keyword_set(hp) THEN BEGIN
        message,/info,/noname, "Usage: photcorrs[, 'infile'][, 'outfile'][, 'parfile']"
        message,/info,/noname, "                [, configuration='photcorrs.conf'][, model='model']"
        message,/info,/noname, "                [, /quiet]"
        message,/info,/noname, "Or:    photcorrs, /manual"
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
    IF size(quiet,/type) NE 0 THEN input = create_struct(input, 'quiet', quiet)
    IF keyword_set(bench) THEN input = create_struct(input, 'benchmark', bench)
    IF NOT keyword_set(conf) THEN conf = d+'photcorrs.conf'
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
        print, 'photcorrs '+version
        print, 'Input file: '+con.infile
        print, 'Output file: '+con.outfile
        print, 'Configuration file: '+conf
        print, 'Input unit: '+strupcase(con.in)
        print, 'Model: '+con.model
        print, 'Reference geometry:'
        print, '    i = '+string(con.refinc)
        print, '    e = '+string(con.refemi)
        print, '    a = '+string(con.refpha)
        print, 'Output unit: '+strupcase(con.out)
        print
    ENDIF

    IF con.benchmark THEN t0 = systime(1)

    ; read the data to be corrected
    iof_data = read_spec(con.infile, error=iof_err_data, wav=wav, info=info, header=iofhdr, quiet=con.quiet)
    ; read photometric model parameters
    p = read_phomodel(con.parfile, con.model, sis=con.phosis, wav=wav, header=parhdr, covar=covar, quiet=con.quiet)
    IF strtrim(sxpar(parhdr, 'FITTED')) EQ 'NO' THEN message, 'Model '+con.model+' is not fitted!'
    IF con.benchmark THEN t1 = benchmark(t0, 'Load data')

    ; convert input data unit to output data unit
    radf = (reff = (brdf = 0))
    iof_data = refquant(iof_data, info.incidang, from=con.in, /radf)
    iof_err_data = refquant(iof_err_data, info.incidang, from=con.in, /radf)
    IF con.benchmark THEN t2 = benchmark(t1, 'Process data unit')

    ; apply photometric correction wavelength by wavelength
    tmp = size(iof_data, /dimension)
    nwv = tmp[0]  ; number of wavelengths
    npt = tmp[1]  ; number of data points
    ngeo = n_elements(con.refpha)  ; number of reference geometries
    iof_corr = fltarr(nwv, npt, ngeo)
    iof_err_corr = iof_corr
    emptydata = fltarr(npt, ngeo)
    FOR i=0, n_elements(wav)-1 DO BEGIN
        IF (where(p[i,*] NE 0))[0] EQ -1 THEN BEGIN
            corr = emptydata
            err = emptydata
        ENDIF ELSE BEGIN
            iof = iof_data[i,*]
            err = iof_err_data[i,*]
            corr = photomet(con.model, p[i,*], iof, info.phaseang, info.incidang, info.emissang, refpha=con.refpha, refinc=con.refinc, refemi=con.refemi, err=err, /radf)
        ENDELSE
        iof_corr[i,*,*] = corr
        iof_err_corr[i,*,*] = err
    ENDFOR
    ; adjust for output units
    FOR k=0, ngeo-1 DO IF con.out[k] NE 'radf' THEN BEGIN
            reff = (brdf = 0)
            IF con.out[k] EQ 'reff' THEN reff = 1
            IF con.out[k] EQ 'brdf' THEN brdf = 1
            iof_corr[*,*,k] = refquant(iof_corr[*,*,k], con.refinc[k], from='radf'  , reff=reff, brdf=brdf)
            iof_err_corr[*,*,k] = refquant(iof_err_corr[*,*,k], con.refinc[k], from='radf', reff=reff, brdf=brdf)
    ENDIF

    IF con.benchmark THEN t3 = benchmark(t2, 'Photometric correction')

    ; propagate keyword values to output
    prop = {}
    ; propagate from input I/F data
    keys = ['obsstart', 'obsend', 'sclkstrt', 'sclkend', 'atltgtid', 'sciseqid', 'seqdesc', 'geofile', 'pxbnfil', 'wvlngths', 'spectyp', 'in_temp', 'thermex']
    FOR i=0, n_elements(keys)-1 DO prop = create_struct(prop, keys[i], sxpar(iofhdr, keys[i]))
    ; propagate from input phopar file
    prop = create_struct(prop, 'spdif', sxpar(parhdr, 'spdif'))
    ; propagate from configuration file
    prop = create_struct(prop, 'mnbest', con.model)
    prop = create_struct(prop, 'in_file', file_basename(con.infile))
    prop = create_struct(prop, 'in_unit', con.in)
    prop = create_struct(prop, 'phtmodfl', file_basename(con.parfile))
    ; other keys
    prop = create_struct(prop, 'pc_sft', 'photcorrs.pro')
    prop = create_struct(prop, 'pc_ver', version)
    prop = create_struct(prop, 'idl_ver', !version.release)
    prop = create_struct(prop, 'os_name', !version.os_name)
    prop = create_struct(prop, 'sysarch', !version.arch)
    prop = create_struct(prop, 'intvrsnb', intvrsnb)
    prop = create_struct(prop, 'iofwl1', 0.391999)    
    prop = create_struct(prop, 'iofwl2', 4.340000)


    ; save output spectral data
    keys2 = ['refunit', 'refinc', 'refemi', 'refpha']
    tags2 = ['out', 'refinc', 'refemi', 'refpha']
    contags = strlowcase(tag_names(con))
    FOR k=0, ngeo-1 DO BEGIN
        ; propagate keys from configuration file
        prop1 = prop
        FOR i=0, n_elements(keys2)-1 DO $
            prop1 = create_struct(prop1, keys2[i], con.(where(contags EQ tags2[i]))[k])
        ; save corrected data
        outfile = strmid(con.outfile,0,strpos(con.outfile,'.',/reverse_search))+'_'+con.out[k]+'.fits'
        write_spec, outfile, iof_corr[*,*,k], iof_err_corr[*,*,k], wav, info, prop=prop1, sis=con.specsis, overwrite=con.overwrite
    ENDFOR

    IF con.benchmark THEN t4 = benchmark(t3, 'All tasks')

    status = 0

END
