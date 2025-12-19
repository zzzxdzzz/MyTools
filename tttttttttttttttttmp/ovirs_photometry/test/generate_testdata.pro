; Script to generate test data files


cd, '..'
workdir = 'test/'
datafile = 'test/test_data.fits'
spawn, 'uname -s', os

print, '`photmods`'
parfile0 = workdir+'phopar_test.fits'
parfile1 = workdir+os+'_phopar_test.fits'
photmods, datafile, parfile0, /bench

models = ['Lommel-Seeliger','McEwen','Minnaert','Akimov']
FOR i=0, 3 DO BEGIN
    print
    print, '`photcorrs.pro` with ',models[i], ' model.'
    corrfile1 = workdir+os+'_corr_test_'+models[i]+'.fits'
    photcorrs, datafile, corrfile1, parfile0, model=models[i], /bench
    print
    print, '`boloalbedos.pro` with ', models[i], 'model.'
    albfile1 = workdir+os+'_bolo_test_'+models[i]+'.fits'
    boloalbedos, datafile, albfile1, parfile0, model=models[i], /bench
ENDFOR

file_move, parfile0, parfile1, /overwrite
qfile0 = (strsplit(parfile0,'.',/ext))[0]+'_qua.fits'
qfile1 = (strsplit(parfile1,'.',/ext))[0]+'_qua.fits'
file_move, qfile0, qfile1, /overwrite

END
