;+
; Test script for OVIRS photometry software package.
; It tests three software tools: photmods, photcorrs, and boloalbedos
;
; Optional Parameters
; ===================
; skip_fits - If set, then skip the comparisons of output FITS fiel with
;             test standards
; stay      - If set, then after all the programs
;
; 1/12/2017, JYL @PSI
; March 28, 2017, JYL @PSI
;   Use status code to check whether software test passes or failed
;-

PRO test_phot, status=status, skip_fits=skip_fits, stay=stay

    IF NOT keyword_set(skip_fits) THEN skip_fits = 0
    IF NOT keyword_set(stay) THEN stay = 0

    ; preparation
    cd,'..'
    testdir = 'test/'
    testdata = 'test_data.fits'
	; testdata = 'temp/g_1900cm_20170503_sopie_spdif_v001_sis.fits'
    testpar = 'temp/phopar_test.fits'
    testparq = 'temp/phopar_test_qua.fits'
    testcorr1 = 'temp/corr_test_reff.fits'
    testcorr2 = 'temp/corr_test_radf.fits'
    testbolo = 'temp/bolo_test.fits'
    exclude = ['DATE','IDL_VER','OS_NAME']
    status = 0
    spawn, 'uname -s', os

    ; test photmods
    print, 'Testing photmods...'
    print, 'IDL> photmods, /help'
    photmods, /help
    print
    print, 'IDL> photmods, status=status, /benchmark'
    photmods, status=s, /benchmark
    status = status OR s
    print
    IF s EQ 0 THEN BEGIN
        print, 'photmods test successful!'
        IF NOT skip_fits THEN BEGIN
            fits_diff, testdir+testpar, testdir+os+'_'+file_basename(testpar), exclude=exclude, diff=d1, /quiet
            fits_diff, testdir+testparq, testdir+os+'_'+file_basename(testparq), exclude=exclude, diff=d2, /quiet
            IF ~d1 AND ~d2 THEN print, 'Output data files match the comparison files'  $
            ELSE BEGIN
                message, /info, /noname, 'photmods output data files do NOT match the comparison files'
                status = status OR 100
            ENDELSE
        ENDIF
	ENDIF ELSE BEGIN
        message, /info, /noname, 'photmods test FAILED!'
        IF NOT stay THEN exit, status=status
    ENDELSE
    print
    print

    ; test photcorrs
    print, 'Testing photcorrs...'
    print, 'IDL> photcorrs, /help'
    photcorrs, /help
    print
    print, 'IDL> photcorrs, status=status, /benchmark'
    models = ['Lommel-Seeliger','McEwen','Minnaert','Akimov']
    FOR i=0, 3 DO BEGIN
        print
        print, 'Testing `photcorrs.pro` with ',models[i], ' model:'
        photcorrs, status=s, model=models[i], /benchmark
        status = status OR s
        print
        IF s EQ 0 THEN BEGIN
            print, 'photcorrs test successful!'
            IF NOT skip_fits THEN BEGIN
                strs = strsplit((strsplit(file_basename(testcorr1),'.',/ext))[0], '_',/ext)
                fits_diff, testdir+testcorr1, testdir+strjoin([os,strs[0:1],models[i],strs[2]],'_')+'.fits', exclude=exclude, diff=d1, /quiet
                strs = strsplit((strsplit(file_basename(testcorr2),'.',/ext))[0], '_',/ext)
                fits_diff, testdir+testcorr2, testdir+strjoin([os,strs[0:1],models[i],strs[2]],'_')+'.fits', exclude=exclude, diff=d2, /quiet
                IF ~d1 AND ~d2 THEN print, 'Output data files match the comparison files'  $
                ELSE BEGIN
                    message, /info, /noname, 'photcorrs output data files do NOT match the comparison files'
                    status = status OR 100
                ENDELSE
            ENDIF
        ENDIF ELSE BEGIN
            message, /info, /noname, 'photcorrs test FAILED!'
            IF NOT stay THEN exit, status=status
        ENDELSE
    ENDFOR
    print
    print

    ; test boloalbedos
    print, 'Testing boloalbedos...'
    print, 'IDL> boloalbedos, /help'
    boloalbedos, /help
    print
    print, 'IDL> boloalbedos, status=status, /benchmark'
    models = ['Lommel-Seeliger','McEwen','Minnaert','Akimov']
    FOR i=0, 3 DO BEGIN
        print
        print, 'Testing `boloalbedos.pro` with ',models[i], ' model:'
        boloalbedos, status=s, model=models[i], /benchmark
        status = status OR s
        IF s EQ 0 THEN BEGIN
            print, 'boloalbedos test successful!'
            IF NOT skip_fits THEN BEGIN
                fits_diff, testdir+testbolo, testdir+os+'_'+(strsplit(file_basename(testbolo),'.',/ext))[0]+'_'+models[i]+'.fits', exclude=exclude, diff=d, /quiet
                IF ~d THEN print, 'Output data file matches the comparison files'  $
                ELSE BEGIN
                    message, /info, /noname, 'boloalbedos output data file does NOT match the comparison file'
                    status = status OR 100
                ENDELSE
            ENDIF
        ENDIF ELSE BEGIN
            message, /info, /noname, 'boloalbedos test FAILED!'
            IF NOT stay THEN exit, status=status
        ENDELSE
    ENDFOR
    print
    print

    print, 'All tests completed.'
    print

    IF NOT stay THEN exit, status=status

end
