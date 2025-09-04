; step5_measure.pro
;	Calculate fft transform from given line
; Steps:
;	Read u,v list from pic file, connect given points into line
;	Open plt map and find e regarding to the (u,v) pints
;	
; Author: XDZ @psi
;
; History:
;	Dec. 18 Created


FUNCTION find_line, file, value
	pic = read_bmp(file)
	im_size = size(pic)
	IF im_size[2] EQ 2 THEN BEGIN
		size1 = im_size[1]
		size2 = im_size[2]
	ENDIF ELSE IF im_size[0] EQ 3 THEN BEGIN
		pic = reform(pic[0,*,*],im_size[2],im_size[3])
		size1 = im_size[2]
		size2 = im_size[3]
	ENDIF

	line=[]
	FOR i=0, size1-1 DO BEGIN
		FOR j=0, size2-1 DO BEGIN
			IF pic[i,j] EQ value THEN line=[[line],[i,j]]
		ENDFOR
	ENDFOR
	RETURN, line
END


PRO corr_r,imap,emap,amap,im,uv,hap_p,r_con,inc,emi,alpha,r,r_cor
	r = []
	inc=[]
	emi=[]
	alpha=[]
	r_cor = []  ; r model
	FOR i =0, n_elements(uv)/2-1 DO BEGIN
		ri		=   im[uv[0,i],uv[1,i]]
		inci	= imap[uv[0,i],uv[1,i]]
		emii	= emap[uv[0,i],uv[1,i]]
		alphai	= amap[uv[0,i],uv[1,i]]
		r=[r,ri]
		inc=[inc,inci]
		emi=[emi,emii]
		alpha=[alpha,alphai]
		r_mi = rfunc_hapke(hap_p[0],hap_p[1],hap_p[2],hap_p[3],hap_p[4],inci,emii,alphai)
		r_cori = ri * r_con / r_mi
		r_cor = [r_cor, r_cori]

	ENDFOR
END

PRO step5p
	savpath = '/Users/ZOU/Desktop/Belton-paper/proj_1/'
	savfile = file_search(savpath,'*.sav')
	hap_p=[0.042, -0.37, 2.5, 0.079, 15]     ; Hapke model parameters from paper
	i_con = 80		; const value for i e alpha
	e_con = 50
	a_con = 37.5

	r_con = rfunc_hapke(hap_p[0],hap_p[1],hap_p[2],hap_p[3],hap_p[4],i_con,e_con,a_con)

	; Read uv points from bmp file
	linesfile = ['/Users/ZOU/Desktop/Belton-paper/proj_1/LINES/pic01.bmp', $
				 '/Users/ZOU/Desktop/Belton-paper/proj_1/LINES/pic02.bmp',  $
				 '/Users/ZOU/Desktop/Belton-paper/proj_1/LINES/pic03.bmp',  $
				 '/Users/ZOU/Desktop/Belton-paper/proj_1/LINES/pic04.bmp',  $
				 '/Users/ZOU/Desktop/Belton-paper/proj_1/LINES/pic05.bmp']
	uv_dots = []
	uv = []
	seg_ptrs=[]		;pointer of uv list for every segment
	FOR i=0, n_elements(linesfile)-1 DO BEGIN
		linefile = linesfile[i]
		uv_dot = find_line(linefile,255)
		uv_dot = reverse(uv_dot,2)				;reverse uv sequence, link pic01 to pic05
		uv_dots = [[uv_dots],[uv_dot]]
		uv_linked = []
		FOR j=1, n_elements(uv_dot)/2-1 DO BEGIN
			uv1 = uv_dot[*,j-1]
			uv2 = uv_dot[*,j]
			linked = linked(uv1,uv2)
			uv_linked = [[uv_linked],[linked]]
			linked_ptr = ptr_new(linked)
			seg_ptrs=[seg_ptrs,linked_ptr]
		ENDFOR
		uv = [[uv],[uv_linked]]
	
	ENDFOR
	
	; restore i,e,alpha,im map
	RESTORE, savfile
;	r = []
;	inc=[]
;	emi=[]
;	alpha=[]
;	r_cor = []  ; r model
;	FOR i =0, n_elements(uv)/2-1 DO BEGIN
;		ri		=   im[uv[0,i],uv[1,i]]
;		inci	= imap[uv[0,i],uv[1,i]]
;		emii	= emap[uv[0,i],uv[1,i]]
;		alphai	= amap[uv[0,i],uv[1,i]]
;		r=[r,ri]
;		inc=[inc,inci]
;		emi=[emi,emii]
;		alpha=[alpha,alphai]
;		r_mi = rfunc_hapke(hap_p[0],hap_p[1],hap_p[2],hap_p[3],hap_p[4],inci,emii,alphai)
;		r_cori = ri * r_con / r_mi
;		r_cor = [r_cor, r_cori]
;
;	ENDFOR
	corr_r,imap,emap,amap,im,uv,hap_p,r_con,inc,emi,alpha,r,r_cor	

	; Try thicken the line
	uv_u1 = uv & uv_u2 = uv
	uv_u1[1,*] = uv_u1[1,*] + 1
	uv_u2[1,*] = uv_u2[1,*] + 2

	uv_d1 = uv & uv_d2 = uv
	uv_d1[1,*] = uv_d1[1,*] - 1
	uv_d2[1,*] = uv_d1[1,*] - 2

	corr_r,imap,emap,amap,im,uv_u1,hap_p,r_con,inc,emi,alpha,r_u1,r_cor_u1
	corr_r,imap,emap,amap,im,uv_u2,hap_p,r_con,inc,emi,alpha,r_u2,r_cor_u2
	corr_r,imap,emap,amap,im,uv_d1,hap_p,r_con,inc,emi,alpha,r_d1,r_cor_d1
	corr_r,imap,emap,amap,im,uv_d2,hap_p,r_con,inc,emi,alpha,r_d2,r_cor_d2

	r_mean	   = (r + r_u1 + r_u2 + r_d1 + r_d2) / 5
	r_cor_mean = (r_cor + r_cor_u1 + r_cor_u2 + r_cor_d1 + r_cor_d2) / 5
	
	; Put uv_dots into r line
	ind = []
	FOR i=0, n_elements(r)-1 DO BEGIN
		FOR j=0, n_elements(uv_dots)/2-1 DO BEGIN
			IF min(uv[*,i] EQ uv_dots[*,j]) EQ 1 THEN BEGIN
				ind = [ind,i]
			ENDIF
		ENDFOR
	ENDFOR

	; For every segment, calculate dist_uv/cos(mean(e)) and mean(r_cor) (from median to 0.025)
	dist_over_e = []
	seg_dist=[]
	r_seg_mean=[]
	r_cor_seg_mean = []
	FOR i=0, n_elements(seg_ptrs)-1 DO BEGIN
		uv_seg = *seg_ptrs[i]
		uv_dist = sqrt((uv_seg[0,0]-uv_seg[0,-1])^2+(uv_seg[1,0]-uv_seg[1,-1])^2)
		seg_dist = [seg_dist, uv_dist]
		e_seg=[]
		r_seg=[]
		r_cor_seg=[]
		FOR j=0,n_elements(uv_seg)/2-1 DO BEGIN
			uv_subd = uv_seg[*,j]
			e_subd = emap[uv_subd[0],uv_subd[1]]
			e_seg=[e_seg,e_subd]

			rj		=   im[uv_subd[0],uv_subd[1]]
			incj	= imap[uv_subd[0],uv_subd[1]]
			emij	= emap[uv_subd[0],uv_subd[1]]
			alphaj	= amap[uv_subd[0],uv_subd[1]]
			r_mj = rfunc_hapke(hap_p[0],hap_p[1],hap_p[2],hap_p[3],hap_p[4],incj,emij,alphaj)
			r_corj = rj * r_con / r_mj
			r_cor_seg = [r_cor_seg, r_corj]
			r_seg = [r_seg, rj]
		ENDFOR
		e_seg_mean=mean(e_seg)
		dist_over_e = [dist_over_e, uv_dist/cos(e_seg_mean * !dtor)]
		r_seg_median = median(r_seg,/even)
		r_cor_seg_median = median(r_cor_seg,/even)
		r_cor_selected=[]
		r_selected=[]
		FOR j =0, n_elements(r_cor_seg)-1 DO BEGIN
			IF [r_cor_seg[j] GT r_cor_seg_median]  THEN BEGIN
				r_cor_selected = [r_cor_selected,r_cor_seg[j]]
			ENDIF
			IF [r_seg[j] GT r_seg_median] AND [r_seg[j] LT 0.03] THEN BEGIN
				r_selected = [r_selected, r_seg[j]]
			ENDIF
		ENDFOR
		IF n_elements(r_cor_selected) GT 0 THEN BEGIN	
			r_cor_seg_mean = [r_cor_seg_mean, mean(r_cor_selected)]
		ENDIF ELSE IF n_elements(r_cor_selected) EQ 0 THEN BEGIN
			r_cor_seg_mean = [r_cor_seg_mean,!values.f_nan]
		ENDIF
		r_seg_mean = [r_seg_mean, mean(r_selected)]
	ENDFOR

	; Plotting
	x_r=indgen(n_elements(r))
	rp = plot(x_r,r,'r',	$
			  ;yrange=[0,0.03], $
			  name='Reflectence profile',$
		      xtitle='<--inner          Profile points          outer-->', $
			  ytitle='Reflectence')
	rpmark = plot(ind,r[ind],'k*',/overplot,name='Layer mark')
	
	;rp = plot(x_r,r_u1,'b',yrange=[0,0.03],name='u1',/overplot)
	;rp = plot(x_r,r_u2,'k',yrange=[0,0.03],name='u2',/overplot)
	;rp = plot(x_r,r_d1,'y',yrange=[0,0.03],name='d1',/overplot)
	;rp = plot(x_r,r_d2,'m',yrange=[0,0.03],name='d2',/overplot)
	rpmean = plot((r+r_u1+r_u2+r_d1+r_d2)/5,'b',/overplot,name = 'Buffed reflectence profile')
	;rpcor = plot(x_r,r_cor,'g',/overplot,name='Corrected reflectence profile')
	!null = legend(target=[rp,rpmark,rpmean])
	
	; Calculating fft for 5 lines using different approch
	; 0  r  fft
	f_c0 = fft(r,/center)
	
	; 1. 5r mean fft
	r_c1 = (r+r_u1+r_u2+r_d1+r_d2) / 5
	f_c1 = fft(r_c1,/center)

	; 2. 5r median fft
	r_all = [[r],[r_u1],[r_u2],[r_d1],[r_d2]]
	r_c2  = median(r_all,dimension=2,/even)
	f_c2  = fft(r_c2,/center)

	; 3. 5r 5fft meam
	f_r	 = fft(r,/center)
	f_ru1 = fft(r_u1,/center)
	f_ru2 = fft(r_u2,/center)
	f_rd1 = fft(r_d1,/center)
	f_rd2 = fft(r_d2,/center)
	f_all = [[f_r],[f_ru1],[f_ru2],[f_rd1],[f_rd2]]
	f_c3 = mean(f_all,dimension=2)

	; 4. 5r 5fft median
	f_c4 = median(f_all,dimension=2,/even)

	fftplot0 = plot(abs(f_c0),'r',name='Reflectence')
	fftplot1 = plot(abs(f_c1),'k',/overplot,name='Buffed reflectence')
	;fftplot = plot(f_c2,'g',/overplot)
	;fftplot = plot(f_c3,'b',/overplot)
	;fftplot = plot(f_c4,'y',/overplot)
	!null = legend(target=[fftplot0,fftplot1])

	; Double y axis
	;dp2 = plot(seg_dist,'b',/overplot,name='UV_dist')
	rpp = plot(r_cor_seg_mean,'g-*', $
			   axis_style = 1,     $
			   margin = [0.15,0.15,0.20,0.15],$
			   xtitle='Layer index',$
			   ytitle='Reflectance',$
			   name='Reflectance corrected')
	rsp = plot(r_seg_mean,'b-*', $
			   /current,       $
			   axis_style = 0, $
			   margin = [0.15,0.15,0.20,0.15],$
			   name='Reflectance')
	dp1 = plot(dist_over_e,'r-*',$ 
			   /current, $
			   margin = [0.15,0.15,0.20,0.15],$
			   axis_style = 0,  $
			   name='Distance')
	a_ppr = axis('y', $
				 target = dp1, $
				 location = [max(dp1.xrange),0,0], $
				 textpos = 1, $
				 title='Distance between layers')
	print, CORRELATE(dist_over_e,r_cor_seg_mean)
	print, CORRELATE(dist_over_e,r_seg_mean)
	!null = legend(target=[dp1,rpp,rsp])

	; Histogram
	hist_dist   = HISTOGRAM(dist_over_e, BINSIZE=1);,location=xbin1)
	hist_corseg = HISTOGRAM(r_cor_seg_mean*3500, BINSIZE=1);,location=xbin2)
	hist_seg    = HISTOGRAM(r_seg_mean*3500, BINSIZE=1);,location=xbin3)

	histplot = PLOT(hist_dist,'r')
	histplot = PLOT(hist_corseg,'g',/overplot)
	histplot = PLOT(hist_seg,'b',/overplot)
	
	; FFT of distance
	fdist = fft(dist_over_e,/center)
	fdp   = plot(abs(fdist),$
				 xtitle = 'Index of frequency')
END

