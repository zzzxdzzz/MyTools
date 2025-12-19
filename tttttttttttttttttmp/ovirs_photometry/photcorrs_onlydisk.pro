
;-

PRO photcorrs_onlydisk
  
  infile1 = '/Users/ZOU/DATA/OSIRIS-REx/OVIRS/test-G-mode/Eq3_oldcal_better-geometry_combined_all.fits'
  infile2 = '/Users/ZOU/DATA/OSIRIS-REx/OVIRS/test-G-mode/Eq3test_2020Feb-cal_l3c_combined_all.fits'
  outfile = '/Users/ZOU/DATA/OSIRIS-REx/OVIRS/test-G-mode/c-ak/Eq3test_2020Feb-cal_l3c_combined_geometry-replaced_disk-corr_ak05-fixed.fits'
 outfile1 = '/Users/ZOU/DATA/OSIRIS-REx/OVIRS/test-G-mode/c-ak/Eq3test_2020Feb-cal_l3c_combined_geometry-replaced_disk-corr_ak1-fixed.fits'

  
  phosis = '/Users/ZOU/WORK/idl/git/sawg/ovirs_photometry/phopar_sis.csv'
  specsis = '/Users/ZOU/WORK/idl/git/sawg/ovirs_photometry/specdata_sis.csv'
                                ;parfile =
                                ;'/Users/ZOU/WORK/OSIRIS-REx/Pub/ZOU-OVIRS/Data/2-SPDIF/spdif/all-spdif-without-refly-v2_par_smoothed.fits'

  
  ;read both old and new files
  iof_data_geo = read_spec(infile1, error=iof_err_data, wav=wav, info=info, header=iofhdr)
  
  iof_data = read_spec(infile2, error=iof_err_data, wav=wav, info=info_t, header=iofhdr_t)
  
   
   
    ; apply photometric correction wavelength by wavelength
    tmp = size(iof_data, /dimension)
    nwv = tmp[0]  ; number of wavelengths
    npt = tmp[1]  ; number of data points
    ngeo = 1
    ak_d = fltarr(nwv,npt)
    ls_d = fltarr(nwv,npt)
    
    iof_corr = fltarr(nwv, npt, ngeo)
    iof_corr_ls= fltarr(nwv, npt, ngeo)
    iof_corr_ak= fltarr(nwv, npt, ngeo)
    iof_corr_ak1= fltarr(nwv, npt, ngeo)
    iof_err_corr = iof_corr
    emptydata = fltarr(npt, ngeo)
    
    FOR i=0, n_elements(wav)-1 DO BEGIN
       iof = iof_data[i,*]
       err = iof_err_data[i,*]
       akd = akimov(info.incidang, info.emissang,info.phaseang, eta = 0.5)
       akd1 = akimov(info.incidang, info.emissang,info.phaseang, eta = 1)
       lsd = 2*lommel_seeliger(info.incidang, info.emissang)
       corr_ak = iof/akd        ;akimov
       corr_ak1 = iof/akd1
       corr_ls = iof/lsd                 ; l-s
       ak_d[i, *] = akd
       ls_d[i, *] = lsd
        
       iof_corr_ak[i,*,*] = corr_ak
       iof_corr_ak1[i,*,*] = corr_ak1
       iof_corr_ls[i,*,*] = corr_ls
     ENDFOR

    ;;;;;;comparing corrected iof (channel wavelength: 0.55)
    plot = plot(iof_corr_ak[79, *,0], info.lat, 'b.',NAME='akimov 0.5',yrange = [-90,90], xrange = [0.02, 0.05],$
                YTITLE='lat', $
                XTITLE='disk-corrected iof', $
                TITLE="comparing corrected iof")
    
    plot = plot(iof_corr_ls[79, *,0], info.lat, 'r.',NAME='L-S',yrange = [-90,90], xrange = [0.01, 0.05], /overplot)
    plot = plot(iof_corr_ak1[79, *,0], info.lat, 'g.',NAME='akimov 1',yrange = [-90,90], xrange = [0.01, 0.05], /overplot)
    leg = LEGEND( /DATA, /AUTO_TEXT_COLOR)

    
    ;;;;;;; Before correction the new-cal data (channel wavelength: 0.55)
    plot2 = plot(iof_data[79, *], info.lat, 'r+',yrange = [-90,90], xrange = [0.005, 0.045],$
                YTITLE='lat', $
                XTITLE='IOF', $
                TITLE="Before correction  0.55")

    ;disk function comparing
    plot3 = plot(lsd, info.lat, 'r+',NAME='L-S',yrange = [-90,90], xrange = [0.85, 1.2] , $
                YTITLE='lat', $
                XTITLE='disk-function, the factor', $
                TITLE="comparing disk function")
    
    plot3 = plot(akd, info.lat, 'b+',NAME='akimov 0.5',yrange = [-90,90],/overplot)
    plot3 = plot(akd1, info.lat, 'g+',NAME='akimov 1',yrange = [-90,90],/overplot)
    leg = LEGEND( /DATA, /AUTO_TEXT_COLOR)


 stop   
   k= 0      
 write_spec, outfile, iof_corr_ak[*,*,k], iof_err_corr[*,*,k], wav, info, prop=prop1, sis=specsis

 write_spec, outfile1, iof_corr_ak1[*,*,k], iof_err_corr[*,*,k], wav, info, prop=prop1, sis=specsis
   

END
