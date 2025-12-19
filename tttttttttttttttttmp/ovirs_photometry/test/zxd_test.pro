PRO zxd_test
;	infile  = './test/test_data.fits'
;	infile  = './test/make-ipdif/NewSPDIF.fits'
;	infile  = './test/IPDIF_ES_APPS.fits'
	infile  = './test/uncertainty/iof_exp1p28_20170321_sopie1.fits'
;	parfile = './test/temp/phopar_test_testdata.fits'
;	parfile = './test/temp/phopar_test_SPDIF.fits'
;	parfile = './test/temp/phopar_test_single.fits'
	parfile = './test/temp/phopar_test.fits'
	
	model1 = 'lommel-seeliger'
	model2 = 'rolo'
	model3 = 'mcewen'
	model4 = 'minnaert'
	model5 = 'akimov'

	p_original = [0.030,-0.0433,0.000264,-0.000000967]

	; Read raw data iof, iof_err, wav and info
	iof_data = read_spec(infile,wav=wav,error=iof_err_data,info=info)
	
	; Creating inc, emi, pha data
	inc = findgen(91)
	emi = fltarr(91)
	pha = inc
	

	; Read model data
	p_data1 = read_phomodel(parfile, model1)
	covar_data1 = mrdfits(parfile,model1+'_Covar')
	p_data2 = read_phomodel(parfile, model2)
	covar_data2 = mrdfits(parfile,model2+'_Covar')
	p_data3 = read_phomodel(parfile, model3)
	covar_data3 = mrdfits(parfile,model3+'_Covar')
	p_data4 = read_phomodel(parfile, model4)
	covar_data4 = mrdfits(parfile,model4+'_Covar')
	p_data5 = read_phomodel(parfile, model5)
	covar_data5 = mrdfits(parfile,model5+'_Covar')

	wav_sample = 400
	p1 = p_data1[wav_sample,*]
	p2 = p_data2[wav_sample,*]
	p3 = p_data3[wav_sample,*]
	p4 = p_data4[wav_sample,*]
	p5 = p_data5[wav_sample,*]

	covar1 = reform(covar_data1[wav_sample,*,*])
	covar2 = reform(covar_data2[wav_sample,*,*])
	covar3 = reform(covar_data3[wav_sample,*,*])
	covar4 = reform(covar_data4[wav_sample,*,*])
	covar5 = reform(covar_data5[wav_sample,*,*])

	sigma1 = fltarr(n_elements(inc))
	sigma2 = sigma1
	sigma3 = sigma1
	sigma4 = sigma1
	sigma5 = sigma1
	FOR i=0,n_elements(pha)-1 DO BEGIN
		; lommel-seeliger
		J1 = [1/p1[0],				$
			  pha[i],		$
			  pha[i]^2,   $
			  pha[i]^3 ]
		; rolo
		D2 = lommel_seeliger(inc[i],emi[i])
		rm2 = rolo_model(pha[i],inc[i],emi[i],p2[0],p2[1],p2[2],p2[3],p2[4],p2[5],p2[6])
		J2 = [D2 * exp(-p2[1] * pha[i]),                  $
			 -p2[0] * pha[i] * D2 * exp(-p2[1] * pha[i]), $
			 D2,                                        $
			 D2 * pha[i],                               $
			 D2 * pha[i]^2,                             $
			 D2 * pha[i]^3,                             $
			 D2 * pha[i]^4] / rm2

		; mcewen
		temp3 = !pi * p3[0] * exp(p3[1]*pha[i] + p3[2]*pha[i]^2 + p3[3]*pha[i]^3)
		rm3 = mcewen_model(pha[i],inc[i],emi[i],p3[0],p3[1],p3[2],p3[3],p3[4],p3[5],p3[6])
		J3    = [1/p3[0], $
			    pha[i],   $
				pha[i]^2, $
				pha[i]^3, $
				pha[i]   - temp3 * cos(inc[i]*!dtor) * pha[i]   / rm3,$
				pha[i]^2 - temp3 * cos(inc[i]*!dtor) * pha[i]^2 / rm3,$
				pha[i]^3 - temp3 * cos(inc[i]*!dtor) * pha[i]^3 / rm3]

		; minnaert
		temp4 = p4[1]*pha[i] + p4[2]*pha[i]^2 + p4[3]*pha[i]^3
		J4 = [1/p4[0],                   $
			 pha[i]   * temp4 / 62.5,   $
			 pha[i]^2 * temp4 / 62.5,   $
			 pha[i]^3 * temp4 / 62.5,   $
			 (p4[4] + p4[5]*pha[i]) / (cos(inc[i]*!dtor)*cos(emi[i]*!dtor)), $
			 (p4[4] + p4[5]*pha[i])*pha[i] / (cos(inc[i]*!dtor)*cos(emi[i]*!dtor))]
		
		; akimov
		rm5= akimov_model(pha[i],inc[i],emi[i],p5[0],p5[1],p5[2],p5[3])
		J5 = [rm5 / p5[0],		$
		     pha[i],	    $
			 pha[i]^2,		$
			 pha[i]^3]
		
		; sigma^2 over r^2
		sigma1[i] = J1 ## covar1 ## transpose(J1)
		sigma2[i] = J2 ## covar2 ## transpose(J2)
		sigma3[i] = J3 ## covar3 ## transpose(J3)
		sigma4[i] = J4 ## covar4 ## transpose(J4)
		sigma5[i] = J5 ## covar5 ## transpose(J5)
	ENDFOR

	; Calculate iof model using given i e pha
	iof_original = phot_model(model1,pha,inc,emi,p_original)
	iof_m1 = phot_model(model1,pha,inc,emi,p1)
	iof_m2 = phot_model(model2,pha,inc,emi,p2)
	iof_m3 = phot_model(model3,pha,inc,emi,p3)
	iof_m4 = phot_model(model4,pha,inc,emi,p4)
	iof_m5 = phot_model(model5,pha,inc,emi,p5)

; PLOTING sigma, sigma/iof_model, iof_model
	; Plotting sigma
	plot1 = plot(sigma1,/ylog,'3r-',      $
				 XTickLen=1.0, YTickLen=1.0 , XGridStyle=1, YGridStyle=1,$
				 yrange = [10e-15,10e10] ,$
				 name = 'Lommel-Seeliger',$
				 xtitle='INC',            $
				 ytitle='Uncertaity',$
		         title='Uncertaity using different models',$
				 font_size=16)
	plot2 = plot(sigma2,/ylog,'3b--',/overplot,name='rolo')
	plot3 = plot(sigma3,/ylog,'3k',  /overplot,name='mcewen')
	plot4 = plot(sigma4,/ylog,'3m',  /overplot,name='minnaert')
	plot5 = plot(sigma5,/ylog,'3g',  /overplot,name='akimov')

	!null = LEGEND(target=[plot1,plot2,plot3,plot4,plot5],position=[95,10e9],/data,font_size=16)
	
	
	; Plotting sigma/iof_model
	plot1 = plot(sigma1/iof_m1,/ylog,'3r-',      $
				 XTickLen=1.0, YTickLen=1.0 , XGridStyle=1, YGridStyle=1,$
				 yrange = [10e-15,10e10] ,$
				 name = 'Lommel-Seeliger',$
				 xtitle='INC',            $
				 ytitle='Uncertaity/iof_model',$
		         title='Uncertaity/iof_model using different models',$
				 font_size=16)
	plot2 = plot(sigma2/iof_m2,/ylog,'3b--',/overplot,name='rolo')
	plot3 = plot(sigma3/iof_m3,/ylog,'3k',  /overplot,name='mcewen')
	plot4 = plot(sigma4/iof_m4,/ylog,'3m',  /overplot,name='minnaert')
	plot5 = plot(sigma5/iof_m5,/ylog,'3g',  /overplot,name='akimov')

	!null = LEGEND(target=[plot1,plot2,plot3,plot4,plot5],position=[95,10e9],/data,font_size=16)
	; Plotting iof_model
	plot1 = plot(iof_m1,'3r-',      $
				 XTickLen=1.0, YTickLen=1.0 , XGridStyle=1, YGridStyle=1,$
				 name = 'Lommel-Seeliger',$
				 xtitle='INC',            $
				 ytitle='I/F',$
		         title='Calculated iof_model using different models',$
				 font_size=16)
	plot2 = plot(iof_m2,'3b--',/overplot,name='rolo')
	plot3 = plot(iof_m3,'3k',  /overplot,name='mcewen')
	plot4 = plot(iof_m4,'3m',  /overplot,name='minnaert')
	plot5 = plot(iof_m5,'3g',  /overplot,name='akimov')
	plot6 = plot(iof_original,'3k--',/overplot,name='original')

	!null = LEGEND(target=[plot1,plot2,plot3,plot4,plot5,plot6],position=[95,0.019],/data,font_size=16)
END

