; Script to process EGA data
;
; 11/9/2017, JYL @PSI
pro ega_processing
workdir = '/Users/ZOU/WORK/idl/git/sawg/ovirs_photometry/test/'


; photometric modeling
datafile = 'ovirs_sci_level3c.fits'
phofile = 'phopar_EGA.fits'
photmods, workdir+datafile, workdir+phofile, /bench

; photometric correction
sufx = ['ls','mce','minn','aki']
models = ['Lommel-Seeliger','McEwen','Minnaert','Akimov']
for i=0,n_elements(models) do begin &$
    corrfile = 'earth_'+sufx[i]+'_corr.fits' &$
    photcorrs, workdir+datafile, workdir+corrfile, workdir+phofile, model=models[i], /bench &$
endfor

; bolometric Bond albedo
sufx = ['ls','mce','aki']
models = ['Lommel-Seeliger','McEwen','Akimov']
for i=0,n_elements(models)-1 do begin &$
    bolofile = 'earth_'+sufx[i]+'_bolo.fits' &$
    boloalbedos, workdir+datafile, workdir+bolofile, workdir+phofile, model=models[i], /bench &$
endfor
end
