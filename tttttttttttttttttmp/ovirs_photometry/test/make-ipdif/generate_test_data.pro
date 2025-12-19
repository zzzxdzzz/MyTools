function dotp, v1, v2
  return, total(v1 * v2,1)
end


;;
;;
;;  For organization of 'FROM_SPEC_INFO', spectral info data must be passed in the 
;;  SPEC_INFO keyword, which is an array of structures of the following format:
;;
;;  {NAME: <string>
;;   PCT: <float>
;;   LON: <float>
;;   LAT: <float>
;;   ANGLE: <float}
;;
;;  defined as follows:
;;
;;  NAME: name of the file containing the spectral info for this spectrum
;;  PCT: percentage of the body surface covered by this spot (currently just informational)
;;  LON/LAT/ANGLE: defines the extent of the spot, but there are two possibile ways of
;;                 doing this
;;
;;  1) circular spot
;;     LAT: single floating point value that is the latitude of the center of the spot (in degrees)
;;     LON: single floating point value that is the longitude of the center of the spot (in degrees)
;;     ANGLE: radius of the spot on the surface (in degrees).  For example, a spot centered at
;;            lat/lon = [0,0] with a radius of 10 degrees, would be a circle with the perimeter
;;            going through (lat=0, lon=10), (lat=10, lon=0), (lat=0, lon=-10), (lat=-10, lon=0). 
;;
;;  2) lat/lon range
;;     LAT: array of two floating point values specifying min and max latitude (in degrees)
;;     LON: array of two floating point values specifying min and max longitude (in degrees)
;;     ANGLE: ignored
;;

pro generate_test_data, sis_file, sclk_file, spectrum_file, outfile=outfile, prop=prop, overwrite=overwrite, nrows=nrows, verbose=verbose, bond=bond, isolate_comments=isolate_comments, phot=phot, spec_dir=spec_dir,sclk_file_type_in=sclk_file_type_in, boresight=boresight, organization_in=organization_in, noise_in=noise_in, snr_in=snr_in, exp_in=exp_in, spec_info=spec_info, icd=icd, debug=debug

  ;; define colors for debug plots
  red = 255l
  green = 255l * 256l
  blue = 255l * 256l * 256l
  magenta = red + blue
  cyan = green + blue
  yellow = red + green

  noise = keyword_set(noise_in) ? noise_in : 0
  snr = keyword_set(snr_in) ? snr_in : 0
  exp = keyword_set(exp_in) ? exp_in : 0
  
  ;; if noise is set to 1 (/noise), make it 2% (0.02)
  if noise eq 1 then noise = 0.02

  organization = keyword_set(organization_in) ? strupcase(organization_in) : 'RANDOM'

  ;; default sclk file type is CSV
  sclk_file_type = keyword_set(sclk_file_type_in) ? strupcase(sclk_file_type_in) : 'CSV'

  if not keyword_set(outfile) then begin
    outfile = keyword_set(bond) ? 'bond' : 'pciof' + (keyword_set(phot) ? '_phot.fits' : '.fits')
  endif

  if not keyword_set(spec_dir) then spec_dir = file_dirname(spectrum_file)

  ;; read in ovirs wavelengths
  readcol,'ovirs_wvl.txt',wvl,count=nwvl,format='d'

  if 0 then begin
    ;; add extra wavlengths to get up to 1400
    wvl = [wvl,wvl[nwvl-1]+(indgen(7) + 1) * (wvl[nwvl-1] - wvl[nwvl-2])]
    nwvl = n_elements(wvl)
  endif

  ;; make wavelength array floating point
  wvl = float(wvl)

  if not keyword_set(bond) then begin
    ;; read in spectral data
    readcol,spectrum_file,swvl,sval,format='f,f',count=nswvl

    if keyword_set(phot) then begin
      ;; get all spectrum files that are not the default one (spectrum_file)
      spec_files = file_search(spec_dir, '*txt')      
      spec_files = spec_files[where(spec_files ne spectrum_file, n_spec)]

      ;; data for all 'other' lab spectra
      spec_data = fltarr(nswvl, n_spec)

      for i=0,n_spec-1 do begin
        readcol,spec_files[i],tmp_swvl,tmp_sval,format='f,f'
        spec_data[*,i] = tmp_sval
      endfor
    endif

    if nswvl lt nwvl then begin
      ;; add additional wavelengths with value equal to last wavelength
      sval = [sval,fltarr(nwvl-nswvl)+sval[nswvl-1]]
      if keyword_set(phot) then spec_data = [spec_data,fltarr(nwvl-nswvl,n_spec)+ rebin(spec_data[nswvl-1,*], nwvl-nswvl, n_spec)]
    endif else begin
      ;; if more wavelengths than our instrument, remove wavelengths from end 
      if nswvl gt nwvl then begin
        sval = sval[0:nwvl-1]
        if keyword_set(phot) then spec_data = spec_data[0:nwvl-1,*]
      endif
    endelse

  endif

  ;; read in sclk data
  case sclk_file_type of 
    'TSV' : readcol,sclk_file,emission,incidence,lat,lon,phase,sclk,format='x,x,x,x,x,x,x,x,x,x,d,x,x,x,x,x,x,x,x,x,x,x,x,d,d,x,d,d,x,x,x,a,x,x,x',delim=string(9b),count=n
    'TSV_1' : readcol,sclk_file,utc,sclk,lat,lon,loc_time,incidence,emission,phase,format='a,a,d,d,a,d,d,d',count=n
    'CSV' : begin
      data = read_csv(sclk_file)
      utc = data.field01
      sclk = data.field02
      lat = keyword_set(boresight) ? data.field03 : data.field12
      lon = keyword_set(boresight) ? data.field04 : data.field13
      loc_time = keyword_set(boresight) ? data.field05 : data.field14
      incidence = keyword_set(boresight) ? data.field06 : data.field15
      emission = keyword_set(boresight) ? data.field07 : data.field16
      phase = keyword_set(boresight) ? data.field08 : data.field17

      n = n_elements(phase)
    end
    else : begin
    end
  endcase

  ;; make sure SCLK strings are padded to 20 characters (add spaces to the right)
  sclk = string(sclk, format='(a-20)')

  ;; number of rows
  nn = keyword_set(nrows) ? nrows : n

  if not keyword_set(bond) then begin
    ;; replicate the spectral data
    data = rebin(sval, nwvl, nn)
    uncertainty = fltarr(nwvl, nn)
  endif

  ;; cut or replicate the sclk data if necessary
  if n ne nn then begin
    ;; replicate (double) the arrays until they are larger than we need
    while nn gt n do begin
      sclk = [sclk,sclk]
      lat = [lat,lat]
      lon = [lon,lon]
      incidence = [incidence,incidence]
      emission = [emission,emission]
      phase = [phase,phase]

      n *=2
    endwhile

    ;; extract only the number of rows we need
    sclk = sclk[0:nn-1]
    lat = lat[0:nn-1]
    lon = lon[0:nn-1]
    incidence = incidence[0:nn-1]
    emission = emission[0:nn-1]
    phase = phase[0:nn-1]
  endif

  if keyword_set(phot) then begin
    ;; for debuggin purposes, save which
    ;; spectrum was used for each measurement
    tmp_spec = bytarr(nn)

    case organization of
      'FROM_SPEC_INFO' : begin
        spec_names = file_basename(spec_files)
        n_spec_2 = n_elements(spec_info)

        ;; if just 1 element in lat/lon then spec_info data
        ;; defines circular spots, if 2 elements, then 
        ;; min/max lat and lon are specified
        circ = n_elements(spec_info[0].lat) eq 1

        if circ then begin
          ;; convert spec_info lon/lat to xyz
          spec_llr = [transpose(spec_info.lon), transpose(spec_info.lat), fltarr(1,n_spec_2) + 1]
          spec_xyz = cv_coord(from_sph=spec_llr, /to_rect, /degrees)
        endif

        spec_inds = lonarr(n_spec_2)
        for i=0,n_spec_2 - 1 do begin
          sp_ind = where(spec_names eq spec_info[i].name)
          spec_inds[i] = sp_ind[0]
        endfor

        ;; process the data row by row (ie: observation by observation)
        for row = 0l,nn-1 do begin
          ;; ignore missing data
          if(phase[row] ne 0 or incidence[row] ne 0 or emission[row] ne 0) then begin
            ;; convert data row lon/lat to xyz
            if circ then $
             row_xyz = cv_coord(from_sph = [lon[row],lat[row],1], /to_rect, /degrees)
            
            ;; check each spectral spot (in order) to see if this observation
            ;; is in the spectral spot
            in_spot = 0
            for i=0,n_spec_2 - 1 do begin
              if circ then begin
                ;; circlar style spots

                ;; find angle between data row xyz and spec_info xyz
                ang = acos(dotp(row_xyz, spec_xyz[*,i])) * 180 / !pi

                ;; if the angle is within the given range, use this spectrum
                if ang lt spec_info[i].angle then in_spot = 1b
              endif else begin
                ;; lat/lon range style spots

                if (lon[row] ge spec_info[i].lon[0] and $
                    lon[row] le spec_info[i].lon[1] and $
                    lat[row] ge spec_info[i].lat[0] and $
                    lat[row] le spec_info[i].lat[1]) then in_spot = 1b
              endelse

              ;; if in this spot then set the
              ;; data to the desired spectral type and drop out of the loop
              if in_spot then begin
                data[*,row] = spec_data[*,spec_inds[i]]
                break
              endif

            endfor

            ;; for debuggin purposes, save which
            ;; spectrum was used for each measurement
            tmp_spec[row] = i + 1
          endif
        endfor

        if keyword_set(debug)then begin
          colors = [red, green, blue, cyan, magenta, yellow]
          nc = n_elements(colors)

          ;; default spectrum

          for j=0,1 do begin
            window, j

            if j eq 0 then begin
              lat_range = [-90,90]
              lon_range = [-180,180]
            endif else begin
              ;; find lat/lon range for all of the "other" spectra
              w = where(tmp_spec ge 1 and tmp_spec le n_spec_2)
              lon_range = [min(lon[w]), max(lon[w])]
              lon_border = (lon_range[1] - lon_range[0]) * 0.1
              lon_range += lon_border * [-1, 1]

              lat_range = [min(lat[w]), max(lat[w])]
              lat_border = (lat_range[1] - lat_range[0]) * 0.1
              lat_range += lat_border * [-1, 1]
            endelse

            w = where(tmp_spec gt n_spec_2)
            plot, lon[w], lat[w], psym=4, thick=j+1, xrange=lon_range, yrange=lat_range, $
             xstyle=1-j, ystyle=1-j

            for i=0,n_spec_2 - 1 do begin
              color = colors[i mod nc]

              if not circ then begin
                oplot, [spec_info[i].lon[0], spec_info[i].lon[1], spec_info[i].lon[1], $
                        spec_info[i].lon[0], spec_info[i].lon[0]], $
                 [spec_info[i].lat[0], spec_info[i].lat[0], spec_info[i].lat[1], $
                  spec_info[i].lat[1], spec_info[i].lat[0]], color=color, thick=j+1
              endif

              w = where(tmp_spec eq i + 1, cnt)
              if cnt gt 0 then begin
                oplot, lon[w], lat[w], psym=4, thick=j+1, color=color
              endif
            endfor
          endfor
        endif
      end

      'RANDOM' : begin
        ;; stick runs (of 8 to 12 each) of each of the "other" lab spectra at random (non-overlapping) locations in the
        ;; default data
        min_run_ln = 8
        max_run_ln = 12
        run_rnd = max_run_ln + 1 - min_run_ln

        nloc = nn / max_run_ln
        loc = indgen(nloc)
        rnd = sort(randomu(seed,nloc))

        for i=0,n_spec-1 do begin
          run_ln = min_run_ln + fix(randomu(seed) * run_rnd)
          inds = loc[rnd[i]] * max_run_ln + indgen(run_ln)

          ;; put in the run of spectrum (i)
          data[*,inds] = rebin(spec_data[*,i], nwvl, run_ln)
        endfor
      end

      'RANDOM2' : begin
        spec_names = file_basename(spec_files)

        max_run_ln = ceil(nn * max(spec_info.pct))
        nloc = nn / max_run_ln
        loc = indgen(nloc)
        rnd = sort(randomu(seed,nloc))

        n_spec_2 = n_elements(spec_info)
        for i=0,n_spec_2 - 1 do begin
          j = where(spec_names eq spec_info[i].name)
          j = j[0]
          
          run_ln = round(nn * spec_info[i].pct)
          inds = loc[rnd[i]] * max_run_ln + indgen(run_ln)

          print, spec_info[i].name, j, run_ln, inds[0]

          ;; put in the run of spectrum (i)
          data[*,inds] = rebin(spec_data[*,j], nwvl, run_ln)
        endfor
        
      end

      'EVEN' : begin
        run_ln =  nn / (n_spec + 1)
        for i=0,n_spec-1 do begin
          inds = run_ln * (i+1) + indgen(run_ln)

          data[*,inds] = rebin(spec_data[*,i], nwvl, run_ln)
        endfor

        ;; fill in the remaining odd rows with the last used spectrum
        ind = max(inds) + 1
        n_left = nn - ind
        if n_left gt 0 then begin
          data[*,ind:*] = rebin(spec_data[*,n_spec-1], nwvl, n_left) 
        endif
      end

      else : begin
      end

    endcase

    ;; de-photometrically correct the data
    data = float(lab_to_iof(data, incidence, emission, phase))

    ;; missing data
    w = where(phase eq 0 and incidence eq 0 and emission eq 0, cnt)
    if cnt gt 0 then  data[*,w]=0
  endif

  ;; made up SCLK data - with reasonable seeming values in most cases

  ;; range to sun of 150,000,000 km
  sun_rng = dblarr(nn) + 150000000
  ;; boresight flag
  bs_flag = bytarr(nn) + 1b
  ;; FOV flag
  fov_flag = bytarr(nn) + 1b
  ;; 
  bs_angle = dblarr(nn)
  ;; range from SC to Bennu
  range = dblarr(nn) + 50
  ;;
  bs_x = dblarr(nn)
  bs_y = dblarr(nn)
  bs_z = dblarr(nn)
  ;;
  fill_fac = dblarr(nn) + 1
  ;; spatial resolution
  smjax = dblarr(nn) + 19
  smnax = dblarr(nn) + 15
  ;; bond albedo
  bond_albedo = dblarr(nn) + 1

  if keyword_set(bond) then begin
    info = {SCLK:sclk[0], LAT:lat[0], LON:lon[0], INCIDENCE:incidence[0], EMISSION:emission[0], PHASE:phase[0], SPATIAL_RESOLUTION:smjax[0], RANGE_TO_SUN:sun_rng[0], BOND_ALBEDO:bond_albedo[0]}
  endif else begin
    info = {SCLK:sclk[0], BS_FLAG:bs_flag[0], FOV_FLAG:fov_flag[0], BS_ANGLE:bs_angle[0], LAT:lat[0], LON:lon[0], RANGE:range[0], BS_X:bs_x[0], BS_Y:bs_y[0], BS_Z:bs_z[0], INCIDANG:incidence[0], EMISSANG:emission[0], PHASEANG:phase[0], FILL_FAC:fill_fac[0], SMJAX:smjax[0], SMNAX:smnax[0], SUN_RNG:sun_rng[0]}
  endelse

  info = replicate(info, nn)

  if keyword_set(bond) then begin
    info.sclk = sclk
    info.lat = lat
    info.lon = lon
    info.incidence = incidence
    info.emission = emission
    info.phase = phase
    info.spatial_resolution = smjax
    info.range_to_sun = sun_rng
    info.bond_albedo = bond_albedo
  endif else begin
    info.sclk = sclk
    info.bs_flag = bs_flag
    info.fov_flag = fov_flag
    info.bs_angle = bs_angle
    info.lat = lat
    info.lon = lon
    info.range = range
    info.bs_x = bs_x
    info.bs_y = bs_y
    info.bs_z = bs_z
    info.incidang = incidence
    info.emissang = emission
    info.phaseang = phase
    info.fill_fac = fill_fac
    info.smjax = smjax
    info.smnax = smnax
    info.sun_rng = sun_rng
  endelse

  if keyword_set(bond) then begin
    jj_write_bolo, outfile, info, sis=sis_file, verbose=verbose, prop=prop, overwrite=overwrite, isolate_comments=isolate_comments
  endif else begin
    if keyword_set(noise) then begin
      uncertainty = data * noise
      noise_mult = 1 + randomn(seed, nwvl, nn) * noise
      data *= noise_mult
    endif else if keyword_set(snr) then begin
      uncertainty = data / snr
      samp = float(snr) ^ 2
      noise_mult = 1 + (samp - randomu(seed, nwvl, nn, poisson=samp)) / samp
      data *= noise_mult
    endif else if keyword_set(exp) then begin      
      noisy_data = ovirs_noise (data, rebin(wvl,nwvl, nn), exp, uncertainty, snr)
      data = noisy_data
    endif

    ;; jj_write_spec2, outfile, data, uncertainty, wvl, info, sis=sis_file, verbose=verbose, prop=prop, overwrite=overwrite, isolate_comments=isolate_comments
    jj_write_spec, outfile, data, uncertainty, wvl, info, sis=sis_file, verbose=verbose, prop=prop, overwrite=overwrite, isolate_comments=isolate_comments, icd=icd
  endelse

  return

  ;; Update some keyword values
  IF keyword_set(prop) THEN BEGIN
    tags = tag_names(prop)
    FOR i=0, n_elements(tags)-1 DO sxaddpar, head, tags[i], prop.(i)
  ENDIF


  mwrfits, data, outfile, header, create=overwrite

  mwrfits, uncertainty, outfile
  mwrfits, wvl, outfile

  ;; tmp_header = load_sis(sis_file, info, h2=h2)
  ;; mwrfits, info, outfile, h2
  
  mwrfits, info, outfile
end
