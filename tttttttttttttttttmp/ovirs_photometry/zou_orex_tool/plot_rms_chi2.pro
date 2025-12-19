pro plot_rms_chi2
  infile = ''
  outfile = ''
  data = read_spec(infile, error=error, wav=wav, info=info, header=header, verbose=verbose)
  tmp = size(data, /dim)
  nwv = tmp[0]
  nsp = tmp[1]
  FOR i=0, n_elements(models)-1 DO BEGIN
     p_model = mrdfits(outfile, i)
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
                plot, models[i], p, iof, iofmod, pha, inc, emi, con, wav=wav[k]
            ENDIF
         ENDFOR
  ENDFOR
  

end
