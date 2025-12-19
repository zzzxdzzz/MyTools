PRO zxd_test1
;	infile  = './test/test_data.fits'
	infile  = './test/ovirs_sci_level3c.fits'
;	infile  = './test/IPDIF_ES_APPS.fits'
;	parfile = './test/temp/phopar_test_testdata.fits'
	parfile = './test/temp/phopar_ovirs_sci_level3c.fits'
;	parfile = './test/temp/phopar_test_single.fits'

	; Default reference i,e,pha
	refinc = 30
	refemi = 0
	refpha = 30

	model1 = 'lommel-seeliger'
	model2 = 'rolo'
	model3 = 'mcewen'
	model4 = 'minnaert'
	model5 = 'akimov'

	p_original = [0.030,-0.0433,0.000264,-0.000000967]

	; Read raw data iof, iof_err, wav and info
	iof_data = read_spec(infile,wav=wav,error=iof_err_data,info=info)
	inc = info.incidang
	emi = info.emissang
	pha = info.phaseang
	
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
	iof = iof_data[wav_sample,*]
	iof_err = iof_err_data[wav_sample,*]
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
	; Contribution of different parameters
	s1 = fltarr(n_elements(p1),n_elements(inc))
	s2 = fltarr(n_elements(p2),n_elements(inc))
	s3 = fltarr(n_elements(p3),n_elements(inc))
	s4 = fltarr(n_elements(p4),n_elements(inc))
	s5 = fltarr(n_elements(p5),n_elements(inc))


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

		s1[*,i] = diag_matrix(covar1) * J1^2 
		s2[*,i] = diag_matrix(covar2) * J2^2
		s3[*,i] = diag_matrix(covar3) * J3^2
		s4[*,i] = diag_matrix(covar4) * J4^2
		s5[*,i] = diag_matrix(covar5) * J5^2

	ENDFOR
	; Calculate iof model using given i e pha
	iof_original = phot_model(model1,pha,inc,emi,p_original)
	iof_m1 = phot_model(model1,pha,inc,emi,p1)
	iof_m2 = phot_model(model2,pha,inc,emi,p2)
	iof_m3 = phot_model(model3,pha,inc,emi,p3)
	iof_m4 = phot_model(model4,pha,inc,emi,p4)
	iof_m5 = phot_model(model5,pha,inc,emi,p5)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Calculate sigma reference
	; Lommel-seeliger
	J1ref = [1/p1[0],  $
			 refpha,   $
			 refpha^2, $
			 refpha^3]
	; Rolo
	D2ref = lommel_seeliger(refinc,refemi)
	rm2ref = rolo_model(refpha,refinc,refemi,p2[0],p2[1],p2[2],p2[3],p2[4],p2[5],p2[6])
	J2ref = [D2ref * exp(-p2[1] * refpha),                  $
			 -p2[0] * refpha * D2ref * exp(-p2[1] * refpha), $
			 D2ref,                                        $
			 D2ref * refpha,                               $
			 D2ref * refpha^2,                             $
			 D2ref * refpha^3,                             $
			 D2ref * refpha^4] / rm2ref
	; Mcewen
	temp3ref = !pi * p3[0] * exp(p3[1]*refpha + p3[2]*refpha^2 + p3[3]*refpha^3)
	rm3ref = mcewen_model(refpha,refinc,refemi,p3[0],p3[1],p3[2],p3[3],p3[4],p3[5],p3[6])
	J3ref = [1/p3[0], $
		    refpha,   $
			refpha^2, $
			refpha^3, $
			refpha   - temp3ref * cos(refinc*!dtor) * refpha   / rm3ref,$
			refpha^2 - temp3ref * cos(refinc*!dtor) * refpha^2 / rm3ref,$
			refpha^3 - temp3ref * cos(refinc*!dtor) * refpha^3 / rm3ref]
	; Minnaert
	temp4ref = p4[1]*refpha + p4[2]*refpha^2 + p4[3]*refpha^3
	J4ref = [1/p4[0],                   $
		 refpha   * temp4ref / 62.5,   $
		 refpha^2 * temp4ref / 62.5,   $
		 refpha^3 * temp4ref / 62.5,   $
		 (p4[4] + p4[5]*refpha) / (cos(refinc*!dtor)*cos(refemi*!dtor)), $
		 (p4[4] + p4[5]*refpha)*refpha / (cos(refinc*!dtor)*cos(refemi*!dtor))]

	 ; Akimov
	rm5ref= akimov_model(refpha,refinc,refemi,p5[0],p5[1],p5[2],p5[3])
	J5ref = [rm5ref / p5[0],		$
		     refpha,	    $
			 refpha^2,		$
			 refpha^3]
	; sigma ref
	sigma1_ref = J1ref ## covar1 ## transpose(J1ref)
	sigma2_ref = J2ref ## covar2 ## transpose(J2ref)
	sigma3_ref = J3ref ## covar3 ## transpose(J3ref)
	sigma4_ref = J4ref ## covar4 ## transpose(J4ref)
	sigma5_ref = J5ref ## covar5 ## transpose(J5ref)
	
	;Plotting
	erroveriof = (iof_err/iof)^2
	; Lommel-seeliger
	plot0 = plot(erroveriof,'3r',name='iof',title='Lommel-seeliger',font_size=16)	
	plot1 = plot(sigma1,'3b',/overplot,name='Lommel-seeliger')
	plot2 = plot(fltarr(n_elements(inc))+sigma1_ref[0],'3g',/overplot,name='sigmaref')
	!null = LEGEND(target=[plot0,plot1,plot2],$
				  /data,font_size=16)
	; rolo
	plot0 = plot(erroveriof,'3r',name='iof',title='rolo',font_size=16)	
	plot1 = plot(sigma2,'3b',/overplot,name='rolo')
	plot2 = plot(fltarr(n_elements(inc))+sigma2_ref[0],'3g',/overplot,name='sigmaref')
	!null = LEGEND(target=[plot0,plot1,plot2],$
				  /data,font_size=16)

	; Mcewen
	plot0 = plot(erroveriof,'3r',name='iof',title='Mcewen',font_size=16)	
	plot1 = plot(sigma3,'3b',/overplot,name='Mcewen')
	plot2 = plot(fltarr(n_elements(inc))+sigma3_ref[0],'3g',/overplot,name='sigmaref')
	!null = LEGEND(target=[plot0,plot1,plot2],$
				  /data,font_size=16)

	; Minnaert
	plot0 = plot(erroveriof,'3r',name='iof',title='Minnaert',font_size=16)	
	plot1 = plot(sigma4,'3b',/overplot,name='Minnaert')
	plot2 = plot(fltarr(n_elements(inc))+sigma4_ref[0],'3g',/overplot,name='sigmaref')
	!null = LEGEND(target=[plot0,plot1,plot2],$
				  /data,font_size=16)

	; Akimov
	plot0 = plot(erroveriof,'3r',name='iof',title='Akimov',font_size=16)	
	plot1 = plot(sigma5,'3b',/overplot,name='Akimov')
	plot2 = plot(fltarr(n_elements(inc))+sigma5_ref[0],'3g',/overplot,name='sigmaref')
	!null = LEGEND(target=[plot0,plot1,plot2],$
				  /data,font_size=16)


	

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;	; Calculate contribution of each parameter in sigma
;	plot1 = plot(s1[0,*]/sigma1,/ylog, '2r',$
;				 XTickLen=1.0, YTickLen=1.0, XGridStyle=1,YgridStyle=1,$
;				 name='p1', $
;				 title = 'Lommel-Seeliger', $
;				 xtitle='point', $
;				 font_size=16)
;	plot2 = plot(s1[1,*]/sigma1,/ylog,'2b', /overplot,name='p2')
;	plot3 = plot(s1[2,*]/sigma1,/ylog,'2m', /overplot,name='p3')
;	plot4 = plot(s1[3,*]/sigma1,/ylog,'2k', /overplot,name='p4')
;	!null = LEGEND(target=[plot1,plot2,plot3,plot4],/data,font_size=16)
;
;	plot1 = plot(s2[0,*]/sigma2, '2r',$
;				 XTickLen=1.0, YTickLen=1.0, XGridStyle=1,YgridStyle=1,$
;				 name='p1', $
;				 title = 'rolo', $
;				 xtitle='point', $
;				 font_size=16)
;	plot2 = plot(s2[1,*]/sigma2,/ylog,'2b', /overplot,name='p2')
;	plot3 = plot(s2[2,*]/sigma2,/ylog,'2m', /overplot,name='p3')
;	plot4 = plot(s2[3,*]/sigma2,/ylog,'2k', /overplot,name='p4')
;	plot5 = plot(s2[4,*]/sigma2,/ylog,'2g', /overplot,name='p5')
;	plot6 = plot(s2[5,*]/sigma2,/ylog,'2--k', /overplot,name='p6')
;	plot7 = plot(s2[6,*]/sigma2,/ylog,'2--m', /overplot,name='p7')
;	
;	!null = LEGEND(target=[plot1,plot2,plot3,plot4,plot5,plot6,plot7],/data,font_size=16)
;
;; PLOTING sigma, sigma/iof_model, iof_model
;	; Plotting sigma
;	plot1 = plot(sigma1,/ylog,'3r-',      $
;				 XTickLen=1.0, YTickLen=1.0 , XGridStyle=1, YGridStyle=1,$
;				 yrange = [10e-15,10e10] ,$
;				 name = 'Lommel-Seeliger',$
;				 xtitle='INC',            $
;				 ytitle='Uncertaity',$
;		         title='Uncertaity using different models',$
;				 font_size=16)
;	plot2 = plot(sigma2,/ylog,'3b--',/overplot,name='rolo')
;	plot3 = plot(sigma3,/ylog,'3k',  /overplot,name='mcewen')
;	plot4 = plot(sigma4,/ylog,'3m',  /overplot,name='minnaert')
;	plot5 = plot(sigma5,/ylog,'3g',  /overplot,name='akimov')
;
;	!null = LEGEND(target=[plot1,plot2,plot3,plot4,plot5],position=[95,10e9],/data)
;	
;	
;	; Plotting sigma/iof_model
;	plot1 = plot(sigma1/iof_m1,/ylog,'3r-',      $
;				 XTickLen=1.0, YTickLen=1.0 , XGridStyle=1, YGridStyle=1,$
;				 yrange = [10e-15,10e10] ,$
;				 name = 'Lommel-Seeliger',$
;				 xtitle='INC',            $
;				 ytitle='Uncertaity/iof_model',$
;		         title='Uncertaity/iof_model using different models',$
;				 font_size=16)
;	plot2 = plot(sigma2/iof_m2,/ylog,'3b--',/overplot,name='rolo')
;	plot3 = plot(sigma3/iof_m3,/ylog,'3k',  /overplot,name='mcewen')
;	plot4 = plot(sigma4/iof_m4,/ylog,'3m',  /overplot,name='minnaert')
;	plot5 = plot(sigma5/iof_m5,/ylog,'3g',  /overplot,name='akimov')
;
;	!null = LEGEND(target=[plot1,plot2,plot3,plot4,plot5],position=[95,10e9],/data)
;	; Plotting iof_model
;	plot1 = plot(iof_m1,'3r-',      $
;				 XTickLen=1.0, YTickLen=1.0 , XGridStyle=1, YGridStyle=1,$
;				 name = 'Lommel-Seeliger',$
;				 xtitle='INC',            $
;				 ytitle='I/F',$
;		         title='Calculated iof_model using different models',$
;				 font_size=16)
;	plot2 = plot(iof_m2,'3b--',/overplot,name='rolo')
;	plot3 = plot(iof_m3,'3k',  /overplot,name='mcewen')
;	plot4 = plot(iof_m4,'3m',  /overplot,name='minnaert')
;	plot5 = plot(iof_m5,'3g',  /overplot,name='akimov')
;	plot6 = plot(iof_original,'3k--',/overplot,name='original')
;
;	!null = LEGEND(target=[plot1,plot2,plot3,plot4,plot5,plot6],position=[95,0.019],/data,font_size=16)
;
	
END

