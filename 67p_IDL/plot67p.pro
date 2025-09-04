pro plot67p
	profile=read_csv("/Users/ZOU/Desktop/Belton-paper/proj_1/out_pics/data_profile.csv")
	r=profile.field1
	rbuff=profile.field2

                                ; r  fft
        r=r-mean(r)
	fft_r = fft(r)
	
                                ; 5r mean fft
        rbuff=rbuff-mean(rbuff)
        fft_rbuff = fft(rbuff)
        x0 = double(INDGEN(n_elements(fft_r)/2))*1.03
	x1 = double(INDGEN(n_elements(fft_r)))*1.03
        z=x0/471
        
        fftplot0 = plot(z,abs(fft_r),'r',name='Reflectence', XTITLE='Frequency (cycle/m)',ytitle='Fourier transform',layout=[1,2,1], xrange=[0,0.05])
	fftplot1 = plot(z,abs(fft_rbuff),'k',/overplot,name='Buffed reflectence', xrange=[0,0.05])

        !null = legend(target=[fftplot0,fftplot1])

        plot0 = plot(x1,r,'r',name='Reflectence', XTITLE='distance along the profile (m)', ytitle="I/F (w/m^2/SR/nm)",layout=[1,2,2], /current)
	plot1 = plot(x1,rbuff,'k',/overplot,name='Buffed reflectence')

        !null = legend(target=[plot0,plot1])
        write_csv,"/Users/ZOU/Desktop/Belton-paper/proj_1/out_pics/reflectance_profile.csv",profile.field1, x1
        write_csv,"/Users/ZOU/Desktop/Belton-paper/proj_1/out_pics/reflectance_profile_buff.csv",profile.field2, x1 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        profile=read_csv("/Users/ZOU/Desktop/Belton-paper/proj_1/out_pics/data_layer.csv")
        dis=profile.field1
        r=profile.field2*3500
        rcor=profile.field3*3500
        
        ; r  fft
        dis=dis-mean(dis)
        fft_dis = fft(dis)
        
        ; r  fft
        r=r-mean(r)
	fft_r = fft(r)
	
        ; rcor fft
        rcor=rcor-mean(rcor)
        fft_rcor = fft(rcor)

        
        x0 = double(INDGEN(n_elements(fft_r)/2))
	x1 = double(INDGEN(n_elements(fft_r)))
        z=x0/n_elements(r)
        
        fftplot00 = plot(z,abs(fft_r),'r',name='reflectence', XTITLE='Frequency (cycle/layer)',ytitle='Fourier transform',layout=[1,2,1])
        fftplot11 = plot(z,abs(fft_rcor),'g',/overplot,name='corrected reflectence')
        fftplot22 = plot(z,abs(fft_dis),'b',/overplot,name='layer thickness')
        
        !null = legend(target=[fftplot00,fftplot11, fftplot22])

        plot00 = plot(x1,r,'r', XTITLE='Index of layer', ytitle="I/F*3500 and distance (m)",layout=[1,2,2], /current)
        plot11 = plot(x1,rcor,'g',/overplot)
        plot22 = plot(x1,dis,'b',/overplot)

end
