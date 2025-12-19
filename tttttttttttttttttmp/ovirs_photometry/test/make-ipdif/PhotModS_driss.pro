;*********************************************************************************************************************************************************
;*******************************************************PHOTMODS*****************************************************************************************
;*********************************************************************************************************************************************************
;NAME: PhotModS
;AUTHOR: 
;   Driss Takir, Ithaca College, Department of Physics&Astronomy, NY 14850
;   dtakir@ithaca.edu
;Date: 5/01/2015
;Build: 2
;Version:2
;
;Description: This software is designed to photometrically model Bennu spectra, which will be measured by OSIRIS-REx OVIRS instrument, using three models: ROLO, Minnaert, and the Lommel_Seeliger.
;
;Copyright (C) 2014, Driss Takir
;This software is developed to support OSIRIS-REx mission data analysis. Non-OSIRIS-REx members who want to use this software need permission from the authors.

;Modification history:

; Build 1.0
;13 March 2014- v1.0- Original release
;04 April 2014- v2.0 - Delivery of the second version- Added some prompt-user test functions
;01 June 2014- v3.0 - Added some SPOC test routines with John Ivens and Changed the data selection to include phase angle coverage of four stations
;03 September 2014- v4.0- Made some cosmetic changes in preparation to SAWG-SPOC meeting in Tucson
;10 October 2014- v5.0- added more comments to the script


;Build 2.1
;25 Junuary 2015- v6.0- added more comments to the script
;Removed all print statements
;Incorporated new test Bennu-like dataset that includes 1400 channels 

;Build 2.2
;4/15/2015- Incorporated the new FITS file format :2015_04_SAWG-OVIRS_Data_Formats_V7
;         



;********************************************************************************************************************************************
;**********************************************************Lommel_Seeliger Function********************************************************
;********************************************************************************************************************************************

;Based on the Lommel-Seeliger function of Takir (2015)
;This function has four free parameters
FUNCTION lommel_Seeliger, p, phase_angle = phase_angle, i_angle = i_angle, e_angle = e_angle, IOF_data = IOF_data, IOF_err = IOF_err

              uo = cos(i_angle * !pi/180.)      ; incidence angle- convert degress to radians
              u = cos(e_angle * !pi/180.) ; emission angle
              falpha = exp(phase_angle*p[1] + p[2] * phase_angle^2 + p[3]* phase_angle^3 ) ; Takir et al.(2014) phase function
              model = (!pi * p[0]) * ((uo/(u+uo))) * falpha; RADF model
              
              return, (IOF_data - model)/IOF_err  ; weight the fits according to the 1-sigma uncertainties in the data
END

;********************************************************************************************************************************************
;**********************************************************ROLO Function*********************************************************************
;********************************************************************************************************************************************

;Based on Robotic Observatory (ROLO) phase function of Buratti et al. (2011)
;This function has seven free parameters
FUNCTION ROLO, p, phase_angle = phase_angle, i_angle = i_angle, e_angle = e_angle, IOF_data = IOF_data, IOF_err = IOF_err ;  this MPFIT() compute and return the model parameters
          
            uo = cos(i_angle * !pi/180.) ; incidence angle- convert degress to radians
            u = cos(e_angle * !pi/180.) ; emission angle 
            falpha =  p[0]*exp(-p[1]*phase_angle) + p[2]+p[3]*phase_angle + p[4]*phase_angle^2 + p[5]*phase_angle^3 + p[6]*phase_angle^4 ; ROLO phase function
            model = (uo/(u + uo) ) * falpha  ; RADF model
            
            return, (IOF_data-model)/IOF_err; weight the fits according to the 1-sigma uncertainties in the data
END

;********************************************************************************************************************************************
;**********************************************************Minnaert Function***************************************************************
;********************************************************************************************************************************************

;Based on the Minnaert function of Takir et al. (2015)
;This function has seven six parameters
FUNCTION minnaert, p, phase_angle = phase_angle, i_angle = i_angle, e_angle = e_angle, IOF_data = IOF_data, IOF_err = IOF_err;  this MPFIT() compute and return the model parameters

            uo = cos(i_angle* !pi/180)                    ; incidence angle- convert degress to radians
            u = cos(e_angle* !pi/180)     ; emission angle 
            kalpha = p[4] + p[5]*phase_angle              ; linear function of phase angle with a slope of b=p[5] and ko=p[4] at 0 phase angle
            falpha=10^(- (p[1]*phase_angle+p[2]*phase_angle^2+p[3]*phase_angle^3)/2.5); Takir et al.(2014) phase function
            model = !pi *p[0]* falpha * (uo)^kalpha * (u)^(kalpha-1); RADF model
          
            return, (IOF_data-model)/IOF_err                 ; weight the fits according to the 1-sigma uncertainties in the data
END

;*********************************************************************************************************************************************
;**********************************************************Write fit file*********************************************************************
;*********************************************************************************************************************************************

; This function writes fit files with Lommel-Seeliger parameters and a header
FUNCTION write_fit_lommel_seeliger, data

            mkhdr, head,data, /extend ; create a header
            
            sxaddpar, head, 'MISSION', 'OSIRIS-REx', 'Mission: OSIRIS-REx'
            sxaddpar, head, 'HOSTNAME', 'OREx', 'PDS Terminology'
            sxaddpar, head, 'INSTRUM', 'OVIRS', 'Instrument: OSIRIS-REx Visible Infrared Spectrometer'
            sxaddpar, head, 'TARGET', '101955 BENNU', 'Target Object'
            sxaddpar, head, 'ORIGIN', 'SPOC', 'University of Arizona Science Processing and Operations Center'
            sxaddpar, head, 'MPHASE', 'Detailed Survey', 'Mission Phase'
            sxaddpar, head, 'ACTIVITY', 'Instrument Data Collection', "Activity is equivalent to PDS4 'Primary Results Summary'"
            sxaddpar, head, 'SCISEQID', '', 'Instrument_Phase_Activity_Description_Rev'
            sxaddpar, head, 'SEQDESC', '', 'From Description field of Science Sequence ID'
            sxaddpar, head, 'OBSSTART', '', 'Observation sequence start time'
                        
            sxaddpar, head, 'OBSEND', '', 'Observation sequence end time'
            sxaddpar, head, 'SCLKSTRT', '', 'partition/TICKS'
            sxaddpar, head, 'SCLKEND', '', 'partition/TICKS'
            sxaddpar, head, 'PXBNFIL', '', 'Name of pixel binned to wavelength file'
            sxaddpar, head, 'GEOFILE', '', 'Name of associated GEOGEN FILE(s)'
            sxaddpar, head, 'METAKER', '', 'Name of Meta Kernel SPICE file'
            
            sxaddpar, head, 'SPDIF', 'SPDIF_all_Bennu.fits', 'Name of Spectral Photometric Data Information File'
            
            sxaddpar, head, 'MNBEST', '', 'Best Fit Model Name (Lommel-Seeliger, ROLO, Minnaert)'
                       
            sxaddpar, head, 'MODELNM', 'Lommel Seeliger', 'Model Name'
            sxaddpar, head, 'FORM', '[pi * A_LS * EXP(b * alpha + c * alpha^2 + d * alpha^3)] * [ u_o / u_o + u]'; Form of Equation for Lommel-Seeliger Model (I/F)
            sxaddpar, head, 'CHI_SQR','X^2', 'Value for Chi-Squred(measue of goodness of fit of model)
            sxaddpar, head, 'COEFF1', 'A_LS', 'Lommel-Seeliger Albedo'
            sxaddpar, head, 'COEFF2', 'b', 'First term in polynomial expansion of f(alpha)- phase function
            sxaddpar, head, 'COEFF3', 'c', 'Second term in polynomial expansion of f(alpha)- phase function
            sxaddpar, head, 'COEFF4', 'd', 'Third term in polynomial expansion of f(alpha)- phase function
            
            writefits, 'Bennu_Model_Parameters.fits', data, head
END


; This function writes fit files with ROLO parameters and a header
FUNCTION write_fit_ROLO, data 

            mkhdr, head,data, /image ; create a header  
            sxaddpar, head, 'MODELNM', 'ROLO', 'Model Name'
            sxaddpar, head, 'FORM', '[C_0 * EXP(-C_1 * alpha) + A_0 + (A_1 * alpha) + (A_2 * alpha^2) + (A_3 * alpha^3) + (A_4 * alpha^4) ] * [ u_o / u_o + u]'; Form of Equation for ROLO Model (I/F)
            sxaddpar, head, 'CHI_SQR','X^2', 'Value for Chi-Squred(measue of goodness of fit of model
            sxaddpar, head, 'COEFF1', 'C_0', 'Opposition Surge Term 1'
            sxaddpar, head, 'COEFF2', 'C_1', 'Opposition Surge Term 2'
            sxaddpar, head, 'COEFF3', 'A_0', 'First term in polynomial expansion of f(alpha) - phase function'
            sxaddpar, head, 'COEFF4', 'A_1', 'Second term in polynomial expansion of f(alpha) - phase function'
            sxaddpar, head, 'COEFF5', 'A_2', 'Third term in polynomial expansion of f(alpha) - phase function'
            sxaddpar, head, 'COEFF6', 'A_3', 'Fourth term in polynomial expansion of f(alpha) - phase function'
            sxaddpar, head, 'COEFF7', 'A_4', 'Fifth term in polynomial expansion of f(alpha) - phase function'
           
            writefits, 'Bennu_Model_Parameters.fits', data,head, /append

            
END

; This function writes fits files with minnaert parameters and a header
FUNCTION write_fit_minnaert, data

            mkhdr, head,data, /image ; create a header
            sxaddpar, head, 'MODELNM', 'Minnaert', 'Model Name'
            sxaddpar, head, 'FORM', '[pi * A_M * 10^-(b * alpha + c * alpha^2 + d * alpha^3)/2.5] * [ (u_o * u)^(k_o + b_1 *alpha)/u]'; Form of Equation for Minnaert Model (I/F)
            sxaddpar, head, 'CHI_SQR','X^2', 'Value for Chi-Squred(measue of goodness of fit of model)
            sxaddpar, head, 'COEFF1', 'A_M', 'Minnaert Albedo'
            sxaddpar, head, 'COEFF2', 'b', 'First term in polynomial expansion of f(alpha)- phase function
            sxaddpar, head, 'COEFF3', 'c', 'Second term in polynomial expansion of f(alpha)- phase function
            sxaddpar, head, 'COEFF4', 'd', 'Third term in polynomial expansion of f(alpha)- phase function
            sxaddpar, head, 'COEFF5', 'k_o', 'First term in polynomial expansion of cosine functions
            sxaddpar, head, 'COEFF6', 'b_1', 'Second term in polynomial expansion of cosine functions
         
            writefits, 'Bennu_Model_Parameters.fits', data, head, /append
            
END

;****************************************************************************************************************************************************
;**********************************************************MAIN PROGRAM******************************************************************************
;****************************************************************************************************************************************************

; to run the program type at the command line: PhotModS, 'model'
PRO PhotModS, argument
        
           timing = systime(0)                          ; read the time and date
           date1 = strmid(timing, 3, 7)
           date2 = strmid(timing, 19, 23)
           print,  date1, timing 
           print, '************************************************************************************************************************************************' ; Welcome panel
           print, '**********************************************************Welcome to PhotModS!******************************************************************'
           print, '************************************************************************************************************************************************'
           print, "*******************************************To read the manual, type at the command line: PhotModS, 'help'***************************************"
           print, '************************************************************************************************************************************************'
           
           aa=findgen(40)/39.*!pi*2. ; set plots' symbols
           usersym,.5*cos(aa),.5*sin(aa),/fill ; set the size of the data points

           set_plot,'ps'       ; set .ps
           decompose = 0       ; color mode
           loadct, 39
           
           ;*****************************************************************************************************
           ; Set data selection criteria*********These can be modified to optimize models efficiency*************  
           ;*****************************************************************************************************
           
           i_angle_min = 0   ; Icidence angle minimum 
           e_angle_min = 0   ; Emission angle minimum
           phase_angle_min = 0 ; Phase angle minimum
           data_avRes_min = 2.5 ; Average Resolution
                    
           i_angle_max = 75 ; Icidence angle maximum
           e_angle_max = 75 ; Emission angle maximum
           phase_angle_max = 111 ; Phase angle maximum
           data_avRes_max = 5.5 ; Phase angle maximum
           
           ; This is applied only to NEAR VIS data; it may not be applied to Bennu
           FOV_FILL = 0 ; Polygon type: Full=0 ;This is different than OSIRIS_REx OVIRS
           ; For OVIRS: Percent filled flag (1 = full, 0= empty, 2= partial

           
           ;read the manual      
           IF STRCMP(argument, 'help', 5, /FOLD_CASE) EQ 1 THEN BEGIN
                XDISPLAYFILE,'Manual_ModS.txt'                               
           ENDIF
          
           ;read the manual 
           IF STRCMP(argument, 'ReadMeFirst', 5, /FOLD_CASE) EQ 1 THEN BEGIN
                XDISPLAYFILE,'ReadMeFirst.txt'                                
           ENDIF          
         
          Iof_data1 = mrdfits('SPDIF.fits', 0, /silent); I over F (RADF)
          Iof_err1 = mrdfits('SPDIF.fits', 1, /silent) ; I over F uncertainty
          wav = mrdfits('SPDIF.fits', 2, /silent) ; I over F uncertainty
          struct = mrdfits('SPDIF.fits', 3, /silent) ; I over F uncertainty  
          wav= STRTRIM(string(wav, format = '(I)'),2) ; convert float to string
 
          ;********************************************************************************************************************************************
          ;**********************************************************ROLO Model******************************************************************************
          ;*******************************************************************************************************************************************
 
           IF STRCMP(argument, 'ROLO', 5, /FOLD_CASE)  EQ 1 then BEGIN
               
                Result = DIALOG_MESSAGE( 'This routine will overwrite any existing files and directories, would you like to continue?',/center, /question,TITLE='Important!!!' )              
                data_R = fltarr(n_elements(wav), 8)             
                      IF STRCMP(Result, 'Yes', 2, /FOLD_CASE) EQ 1 THEN BEGIN                     
                            FILE_MKDIR, 'ROLO'                             
                            FOR k=0,n_elements(wav)-1  DO BEGIN   ; Access all SADIF access all IPDIFk = 0
                                                              
                                       SCLK = struct.SCLK ; Partition/TICKS
                                       Lat = struct.lat
                                       Lon = struct.lon
                                       i_angle = struct.INCIDENCE
                                       e_angle = struct.EMISSION
                                       phase_angle = struct.PHASE
                                       Spatial_Resolution = struct.SPATIAL_RESOLUTION
                                       range_sun = struct.RANGE_TO_SUN
                                       
                                       IOF_data = Iof_data1[k,*]
                                       IOF_ERR = Iof_err1[k,*]
                                        
                                        ;*********************************************Careful, this where statement assumes that spectra taken on sky are less than 0***********
                                        ;*****************************************************************************************************************************
                                               
                                        in = where(iof_data gt 0 and i_angle gt 0 and i_angle lt 75 and e_angle gt 0 and e_angle lt 75 and phase_angle gt 0 $   ; Data selection
                                        and phase_angle lt 111 and Spatial_Resolution gt 2.5 and Spatial_Resolution lt 5.5 )
                                        
                                        SCLK = SCLK(in)
                                        lat = lat(in)
                                        lon=lon(in)
                                        i_angle=i_angle(in)
                                        e_angle=e_angle(in)
                                        phase_angle=phase_angle(in)
                                        Spatial_Resolution=Spatial_Resolution(in)
                                        IOF_data=IOF_data(in)
                                        IOF_ERR=IOF_ERR(in)
                                        
                                       ;Set the characteristics of the plot 
                                        device,/schoolbook, fil='ROLO/Bennu_ROLO_Fit_'+wav[k]+'.ps',/color,xs=20,ys=25,xoff=1,yoff=1     ; set .ps file     
                                        !p.charsize=1.2        
                                        !p.font=0
                                        !p.multi=[0,1,2]
                                         plot,[0],[0],xra=[0 ,.25],xminor = .5 ,xst=.05,xthi=1.2,psym=3,yra=[0,.25],yst=1,ytit='(I/F)!DModeled!N', charsize = 1, xtit='(I/F)!DMeasured!N',ythi=1,$
                                         yminor=.5,title = 'Asteroid Bennu /Driss Takir/ '+' '+timing ; make a plot
                              
                                         fa = {phase_angle:phase_angle, i_angle:i_angle, e_angle:e_angle, IOF_data:IOF_data, IOF_err:IOF_err}  ; parameters that will be used in the MPFIT()
                                          
                                         guessp = [0,0.2,0.29,0.0007,-0.0001,0.00000001,-0.0000006]    ; initial guess for MPFIT()***********This can be manipulated to get a better fit***************   
                                         p = mpfit('ROLO', guessp, functargs=fa)                          ; generate parameters with MPFIT()
                                         
                                         uo = cos(i_angle * !pi/180)                                      ; convert from degrees to radians and compute uo
                                         u = cos(e_angle * !pi/180)
                                      
                                         falpha = p[0]*exp(-p[1]*phase_angle) + p[2]+p[3]*phase_angle + p[4]*phase_angle^2 + p[5]*phase_angle^3 + p[6]*phase_angle^4 ; phase function of ROLO
                                     
                                         IOF_model = (uo/(u + uo))*falpha                                                       ; IOF_model calculation
                                       
                                         oplot,IOF_data,IOF_model, psym=8, col = 250    ; set a plot
                                       
                                         m = poly_fit(IOF_data, IOF_data,1,/double)     
                                         m1=poly(IOF_data,m)                               ;  calculate the perfect fit (blue line)  
                                         
                                         c = linfit(IOF_data, IOF_model, chisq= chisq_m, /double)
                                         ;R^2
                                       
                                         mean_value = mean(IOF_data)
                                         total = total((iof_data-mean_value)^2)
                                         res = total((iof_data-iof_model)^2)
                                         
                                         R_2 = 1- (res/total)
                                        
                                         m2 = [0,.25]                                     ; set a range for the best fit       
                                         m3 = poly(m2,m)  
                                         oplot, m2, m3, thi=4, linestyle = 0, col = 60    ; plot the perfect fit (bleu line)
                                    
                                         xyouts, 0.04, 0.22, 'ROLO Fit to Bennu Spectra('+wav[k]+' nm)', /data, siz=1.5, col =0     ; plot's labels
                                         xyouts,0.01, 0.2,'I/F = {[f!DR!N(!9a!x)!9m!x!Do!N]/(!9m!x!Do!N+!9m!x)}', /data,siz = 1  
                                         xyouts,.01, .19,'f!DR!N(!9a!x)= C!D0!Nexp-(C!D1!N!9a!x)+A!D0!N+A!D1!N!9a!x+A!D2!N!9a!x!E2!N+A!D3!N!9a!x!E3!N+A!D4!N!9a!x!E4!N',/data,siz=.8, col = 0
                                         xyouts,.15,.12,'C!D0!N='+STRTRIM(string(p[0], format = '(d20.3)'),2),/data,siz=1, col =0            ; plot's labels
                                         xyouts,.15,.11,'C!D1!N ='+STRTRIM(string(p[1], format = '(d20.4)'),2),/data,siz=1, col =0            ; plot's labels
                                         xyouts,.15, .10,'A!D0!N = ' +STRTRIM(string(p[2], format = '(d20.6)'),2),/data,siz=1, col = 0         ; plot's labels
                                         xyouts,.15, .09,'A!D1!N = ' +STRTRIM(string(p[3], format = '(d20.9)'),2),/data,siz=1, col = 0         ; plot's labels
                                         xyouts,.15, .08,'A!D2!N = '+STRTRIM(string(p[4], format = '(d20.12)'),2),/data,siz=1, col = 0          ; plot's labels
                                         xyouts,.15, .07,'A!D3!N = '+STRTRIM(string(p[5], format = '(d20.12)'),2),/data,siz=1, col = 0           ; plot's labels
                                         xyouts,.15, .06,'A!D4!N = '+STRTRIM(string(p[6], format = '(d20.12)'),2),/data,siz=1, col = 0           ; plot's labels  
                                         xyouts, .15, .04, '!9c!x!E2!N=' +STRTRIM(string(chisq_m, format = '(d20.2)'),2),  /data, siz=1, col =0  ; plot's labels
                                         xyouts, .15, .02, 'R!E2!N=' +STRTRIM(string(R_2, format = '(d20.2)'),2),  /data, siz=1, col =0  ; plot's labels
                                         
                                            
                                         xyouts,.01, .17,' Data selection:', /data,siz = 1
                                         xyouts,.01, .155,STRTRIM(string(i_angle_min),2)+'!Eo!N'+' < incidence angle (!16i!X) < ' + STRTRIM(string(i_angle_max),2)+'!Eo!N', /data,siz = .9 ; plot's labels
                                         xyouts,.01, .145,STRTRIM(string(e_angle_min),2)+'!Eo!N'+' < emission angle (!16i!X) < ' + STRTRIM(string(e_angle_max),2)+'!Eo!N', /data,siz = .9 ; plot's labels
                                         xyouts,.01, .135,STRTRIM(string(phase_angle_min),2)+'!Eo!N'+' < phase angle(!9a!x) < ' + STRTRIM(string(phase_angle_max),2)+'!Eo!N', /data,siz = .9 ; plot's labels
                                         xyouts,.01, .125,STRTRIM(string(data_avRes_min, format = '(F0.2)'),2)+'km'+' < spatial resolution < ' + STRTRIM(string(data_avRes_max, format = '(F0.2)'),2)+'km', /data,siz = .9 ; plot's labels
                                  
                                         device,fil='ROLO/Bennu_ROLO_Viewing_geometry_'+wav[k]+'.ps',/color,xs=20,ys=25,xoff=1,yoff=1 ; set .ps file for viewing geometry plots
                                                  
                                         !p.charsize=1.2
                                         !p.font=0
                                         !p.multi=[0,1,3]
                                     
                                          plot,[0],[0],xra=[0 ,120],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Phase Angle(!16!9a!x'+'!Eo!N)',ythi=1, yminor=1
                                          oplot, phase_angle, IOF_model/IOF_data, psym=8, col = 250 ; make a plot
                                       
                                          m3 = [0,150]  
                                          oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60 ; plot the perfect fit (blue line)                                    
                                    
                                          xyouts, 35, 1.4, 'Fit vs. Phase Angle', /data, siz=1.5, col =0
                                     
                                          plot,[0],[0],xra=[0 ,80],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Incidence Angle(!16i!X!Eo!N)',ythi=1, yminor=1 ; set a plot
                                          oplot, i_angle, IOF_model/IOF_data, psym=8, col = 250 ;  plot incidence angle vs. fit
                                          oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60
                          
                                  
                                          xyouts, 18, 1.4, 'Fit vs. Incidence Angle', /data, siz=1.5, col = 0
                                                              
                                          plot,[0],[0],xra=[0 ,80],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Emission Angle(!16e!X!Eo!N)',ythi=1, yminor=1 ; set a plot 
                                          oplot, e_angle, IOF_model/IOF_data, psym=8, col = 250  ; plot emission angle vs. fit
                                          oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60  
                                          xyouts, 19, 1.4, 'Fit vs. Emission Angle', /data, siz=1.5, col =0 
                                          m = poly_fit(e_angle,  replicate(1.0, N_ELEMENTS(e_angle) ),1,/double) ; compute the fit
                                          m1=poly(e_angle,m)
                                           
                                          
                                
                                          parameters = [chisq_m,p[0], p[1], p[2], p[3], p[4], p[5], p[6] ]
        
                 
                                          data_R[k, *] = parameters
                           ENDFOR
                            
                            Result = DIALOG_MESSAGE( 'ROLO models are generated, please check ROLO directory ',/center, /information,TITLE='Information!!')
                            result= write_fit_ROLO(data_R) ; write data in a FITS file
        
                      ENDIF
                      
                     device,/close  
          ENDIF
  
          ;*******************************************************************************************************************************************
          ;**********************************************************Minnaert Model*************************************************************************
          ;*******************************************************************************************************************************************
    
          IF STRCMP(argument, 'minnaert', 5, /FOLD_CASE)  EQ 1 then BEGIN
             
                data_M = fltarr(n_elements(wav), 7)
                Result = DIALOG_MESSAGE( 'This routine will overwrite any existing files and directories, would you like to continue?',/center, /question,TITLE='Important!!!' )
                IF STRCMP(Result, 'Yes', 2, /FOLD_CASE) EQ 1 THEN BEGIN   
                       FILE_MKDIR, 'Minnaert'                  
                                            
                       FOR k=0, n_elements(wav)-1 DO BEGIN   ; access all SADIF access all IPDIF                          
                               SCLK = struct.SCLK ; Partition/TICKS
                               Lat = struct.lat
                               Lon = struct.lon
                               i_angle = struct.INCIDENCE
                               e_angle = struct.EMISSION
                               phase_angle = struct.PHASE
                               Spatial_Resolution = struct.SPATIAL_RESOLUTION
                               range_sun = struct.RANGE_TO_SUN
                       
                               IOF_data = Iof_data1[k,*]
                               IOF_ERR = Iof_err1[k,*]
                                 
                               in = where(iof_data gt 0 and i_angle gt 0 and i_angle lt 75 and e_angle gt 0 and e_angle lt 75 and phase_angle gt 0 $
                               and phase_angle lt 111 and Spatial_Resolution gt 2.5 and Spatial_Resolution lt 5.5 )
                                   
                               SCLK = SCLK(in)
                               lat = lat(in)
                               lon=lon(in)
                               i_angle=i_angle(in)
                               e_angle=e_angle(in)
                               phase_angle=phase_angle(in)
                               Spatial_Resolution=Spatial_Resolution(in)
                               IOF_data=IOF_data(in)
                               IOF_ERR=IOF_ERR(in)
                                 
                               device,/schoolbook, fil='minnaert/Bennu_Minnaert_Fit_'+wav[k]+'.ps',/color,xs=20,ys=25,xoff=1,yoff=1     ; set .ps file
                                  
                               !p.charsize=1.2
                               !p.font=0
                               !p.multi=[0,1,2]
                             
                               plot,[0],[0],xra=[0 ,.25],xminor = .5 ,xst=.05,xthi=1.2,psym=3,yra=[0,.3],yst=1,ytit='(I/F)!DModeled!N', charsize = 1,$
                               xtit='(I/F)!DMeasured!N',ythi=1, yminor=.5,title = 'Asteroid Bennu / Driss Takir / '+' '+timing
             
                               fa = {phase_angle:phase_angle, i_angle:i_angle, e_angle:e_angle, IOF_data:IOF_data, IOF_err:IOF_err}  ; parameters that will be used in the MPFIT()
                               guessp = [.5,0,0,0,0,.05] ; initial guess for MPFIT() and can be changed for a better fits
                               p = mpfit('minnaert', guessp, functargs=fa); generating parameters with MPFIT()
                
                               uo = cos(i_angle * !pi/180) ; convert from degrees to radians and compute uo
                               u = cos(e_angle * !pi/180)
                               
                               kalpha = p[4] + p[5]*phase_angle   
                               falpha=10^(- (p[1]*phase_angle+p[2]*phase_angle^2+p[3]*phase_angle^3)/2.5) ;  phase function of the Minnaert
                    
                                IOF_model = !pi*p[0]* falpha * (uo)^kalpha * (u)^(kalpha-1); IOF_model
                   
                                oplot,IOF_data,IOF_model, psym=8, col = 250; set a plot
                    
                                m = poly_fit(IOF_data, IOF_data,1,/double);  calculate the perfect fit (bleu line)
                                m1=poly(IOF_data,m)
                    
                                c = linfit(IOF_data, IOF_model, chisq= chisq_m, /double)
                                
                                ;R^2
                                
                                mean_value = mean(IOF_data)
                                total = total((iof_data-mean_value)^2)
                                res = total((iof_data-iof_model)^2)
                                
                                R_2 = 1- (res/total)
                                
                   
                                m2 = [0,.23] ; set a range for the best fit
                                m3 = poly(m2,m)
                
                                oplot, m2, m3, thi=4, linestyle = 0, col = 60; plot the perfect fit
                
                                xyouts, .04, .27, 'Minnaert fit to Bennu Spectra('+wav[k]+' nm)', /data, siz=1.5, col =0;               plot's labels
                                xyouts,.007, .23,'(I/F) = [!9p!xA!DM!Nf!DM!N(!9a!x)(!9m!x!Do!N!9m!x)!Ek(!9a!x)!N]/!9m!x', /data,siz = 1.2       
                                xyouts,.007, .215,'f!DM!N(!9a!x)=10^-[(!9b!x!9a!x + !9g!x!9a!x!E2!N +!9d!x!9a!x!E3!N)/2.5]',/data,siz=1, col = 0
                                xyouts,.007, .20,'k(!9a!x)= k!Do!N+b!9a!x',/data,siz=1, col = 0
                                xyouts,.15,.12,'A!DM!N='+STRTRIM(string(p[0], format = '(d20.3)'),2),/data,siz=1, col =0
                                xyouts,.15,.105,'!9b!x ='+STRTRIM(string(p[1], format = '(d20.4)'),2),/data,siz=1, col =0
                                xyouts,.15, .09,'!9g!x = ' +STRTRIM(string(p[2], format = '(d20.6)'),2),/data,siz=1, col = 0
                                xyouts,.15, .075,'!9d!x = ' +STRTRIM(string(p[3], format = '(d20.9)'),2),/data,siz=1, col = 0
                                xyouts,.15, .06,'k!Do!N = '+STRTRIM(string(p[4], format = '(d20.3)'),2),/data,siz=1, col = 0
                                xyouts,.15, .045,'b = '+STRTRIM(string(p[5], format = '(d20.4)'),2),/data,siz=1, col = 0
                                xyouts, .15, .025, '!9c!x!E2!N=' +STRTRIM(string(chisq_m, format = '(d20.2)'),2),  /data, siz=1, col =0 
                                xyouts, .15, .01, 'R!E2!N=' +STRTRIM(string(R_2, format = '(d20.2)'),2),  /data, siz=1, col =0  ; plot's labels
                                
                                   
                                xyouts,.01, .17,' Data selection:', /data,siz = 1
                                xyouts,.01, .155,STRTRIM(string(i_angle_min),2)+'!Eo!N'+' < incidence angle (!16i!X) < ' + STRTRIM(string(i_angle_max),2)+'!Eo!N', /data,siz = .9
                                xyouts,.01, .145,STRTRIM(string(e_angle_min),2)+'!Eo!N'+' < emission angle (!16i!X) < ' + STRTRIM(string(e_angle_max),2)+'!Eo!N', /data,siz = .9
                                xyouts,.01, .135,STRTRIM(string(phase_angle_min),2)+'!Eo!N'+' < phase angle(!9a!x) < ' + STRTRIM(string(phase_angle_max),2)+'!Eo!N', /data,siz = .9
                                xyouts,.01, .125,STRTRIM(string(data_avRes_min, format = '(F0.2)'),2)+'km'+' < spatial resolution < ' + STRTRIM(string(data_avRes_max, format = '(F0.2)'),2)+'km', /data,siz = .9
                                                             
                                device,fil='Minnaert/Bennu_Minnaert_Viewing_geometry_'+wav[k]+'.ps',/color,xs=20,ys=25,xoff=1,yoff=1 ; set .ps file for viewing geometry plots
                                  
                                !p.charsize=1.2
                                !p.font=0
                                !p.multi=[0,1,3]
                     
                                plot,[0],[0],xra=[0 ,120],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Phase Angle(!16!9a!x'+'!Eo!N)',ythi=1, yminor=1
                                oplot, phase_angle, IOF_model/IOF_data, psym=8, col = 250 ; make a plot
                
                                xyouts, 35, 1.4, 'Fit vs. Phase Angle', /data, siz=1.5, col =0;               plot's labels
                
                                m = poly_fit(phase_angle,  replicate(1.0, N_ELEMENTS(phase_angle) ),1,/double); set the perfect fit (bleu line)
                                m1=poly(phase_angle,m)
                                chisq_a= XSQ_TEST(IOF_model/IOF_data, phase_angle)
                                m3 = [0,150]
                
                                oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60  ; make a plot of the perfect fit (bleu line)
                                  
                           
                                plot,[0],[0],xra=[0 ,80],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Incidence Angle(!16i!X!Eo!N)',ythi=1, yminor=1
                
                                oplot, i_angle, IOF_model/IOF_data, psym=8, col = 250  ; make a plot incidence angle vs. IOF_model/IOF_data
                                oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60
                                xyouts, 18, 1.4, 'Fit vs. Incidence Angle', /data, siz=1.5, col = 0
               
                                plot,[0],[0],xra=[0 ,80],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Emission Angle(!16e!X!Eo!N)',ythi=1, yminor=1
                
                                oplot, e_angle, IOF_model/IOF_data, psym=8, col = 250
                                oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60
                                xyouts, 19, 1.4, 'Fit vs. Emission Angle', /data, siz=1.5, col =0
                
                                parameters = [chisq_m,p[0], p[1], p[2], p[3], p[4], p[5] ]
                                data_M[k, *] = parameters
                   
                                 
                        ENDFOR         
                        Result = DIALOG_MESSAGE( 'Minnaert models are generated, please check Minnaert directory ',/center, /information,TITLE='Information!!')
                        result= write_fit_Minnaert(data_M)        
                  ENDIF
                  device,/close
           ENDIF     
       
          ;********************************************************************************************************************************************
          ;**********************************************************Lommel Seeliger*******************************************************************
          ;********************************************************************************************************************************************
          
          IF STRCMP(argument, 'lommel_Seeliger', 5, /FOLD_CASE)  EQ 1 then BEGIN

          data_LS = fltarr(n_elements(wav), 5)
          Result = DIALOG_MESSAGE( 'This routine will overwrite any existing files and directories, would you like to continue?',/center, /question,TITLE='Important!!!' )
          IF STRCMP(Result, 'Yes', 2, /FOLD_CASE) EQ 1 THEN BEGIN
           
                            FILE_MKDIR, 'Lommel_Seeliger'                          
                            FOR k=0, n_elements(wav)-1  DO BEGIN   ; access all SADIF access 
                                
                                SCLK = struct.SCLK ; Partition/TICKS
                                Lat = struct.lat
                                Lon = struct.lon
                                i_angle = struct.INCIDENCE
                                e_angle = struct.EMISSION
                                phase_angle = struct.PHASE
                                Spatial_Resolution = struct.SPATIAL_RESOLUTION
                                range_sun = struct.RANGE_TO_SUN
                            
                                IOF_data = Iof_data1[k,*]
                                IOF_ERR = Iof_err1[k,*]
                             
                                in = where(iof_data gt 0 and i_angle gt 0 and i_angle lt 75 and e_angle gt 0 and e_angle lt 75 and phase_angle gt 0 $
                                  and phase_angle lt 111 and Spatial_Resolution gt 2.5 and Spatial_Resolution lt 5.5 )
                                  
                                SCLK = SCLK(in)
                                lat = lat(in)
                                lon=lon(in)
                                i_angle=i_angle(in)
                                e_angle=e_angle(in)
                                phase_angle=phase_angle(in)
                                Spatial_Resolution = Spatial_Resolution(in)
                                IOF_data=IOF_data(in)
                                IOF_ERR=IOF_ERR(in)
              
                                device,/schoolbook, fil='Lommel_Seeliger/Bennu_Lommel_Seeliger_Fit_'+wav[k]+'.ps',/color,xs=20,ys=25,xoff=1,yoff=1     ; set .ps file
                     
                                !p.charsize=1.2
                                !p.font=0
                                !p.multi=[0,1,2]
            
                                plot,[0],[0],xra=[0 ,.25],xminor = .5 ,xst=.05,xthi=1.2,psym=3,yra=[0,.28],yst=1,ytit='(I/F)!DModeled!N', charsize = 1, xtit='(I/F)!DMeasured!N',ythi=1, yminor=.5,$
                                  title = 'Asteroid Bennu / Driss Takir / '+' '+timing
              
                                fa = {phase_angle:phase_angle, i_angle:i_angle, e_angle:e_angle, IOF_data:IOF_data, IOF_err:IOF_err}  ; parameters that will be used in the MPFIT()
                                guessp = [0,-.0033,.000000018,.00000071]
              
                                p = mpfit('lommel_seeliger', guessp, functargs=fa)
                
                                uo = cos(i_angle * !pi/180)
                                u = cos(e_angle * !pi/180)
                 
                                falpha = exp(phase_angle*p[1] + p[2] * phase_angle^2 + p[3]* phase_angle^3 )  
                                IOF_model = (!pi * p[0]) * (uo/(uo + u)) * falpha
              
                                oplot,IOF_data,IOF_model, psym=8, col = 250
          
                                m = poly_fit(IOF_data, IOF_data,1,/double)
                               
                                c = linfit(IOF_data, IOF_model, chisq= chisq_m, /double)
                                
                                ;R^2
                                
                                mean_value = mean(IOF_data)
                                total = total((iof_data-mean_value)^2)
                                res = total((iof_data-iof_model)^2)
                                
                                R_2 = 1- (res/total)
              
                                m2 = [0,.23]
                                m3 = poly(m2,m)
                                oplot, m2, m3, thi=4, linestyle = 0, col = 60
                
                                xyouts, .02, .25, 'Lommel Seeliger fit to Bennu Spectra('+wav[k]+' nm)', /data, siz=1.5, col =0
                                xyouts,.005, .20,'(I/F) = {!9p!xA!DLS!Nf!DLS!N(!9a!x)[!9m!x!Do!N/(!9m!x!Do!N+!9m!x)]}', /data,siz = 1.2
                                xyouts,.005, .185,'f!DLS!N(!9a!x)=exp(!9b!x!9a!x+!9g!x!9a!x!E2!N+!9d!x!9a!x!E3!N)  ',/data,siz=1, col = 0  
                                xyouts,.15,.10,'A!DLS!N='+STRTRIM(string(p[0], format = '(d20.3)'),2),/data,siz=1, col =0
                                xyouts,.15,.085,'!9b!x ='+STRTRIM(string(p[1], format = '(d20.4)'),2),/data,siz=1, col =0
                                xyouts,.15, .07,'!9g!x = ' +STRTRIM(string(p[2], format = '(d20.6)'),2),/data,siz=1, col = 0
                                xyouts,.15, .055,'!9d!x = ' +STRTRIM(string(p[3], format = '(d20.9)'),2),/data,siz=1, col = 0
                                xyouts, .15, .03, '!9c!x!E2!N=' +STRTRIM(string(chisq_m, format = '(d20.2)'),2),  /data, siz=1, col =0
                                xyouts, .15, .01, 'R!E2!N=' +STRTRIM(string(R_2, format = '(d20.2)'),2),  /data, siz=1, col =0  ; plot's labels
                                
                                
                                xyouts,.01, .17,' Data selection:', /data,siz = 1
                                xyouts,.01, .155,STRTRIM(string(i_angle_min),2)+'!Eo!N'+' < incidence angle (!16i!X) < ' + STRTRIM(string(i_angle_max),2)+'!Eo!N', /data,siz = .9
                                xyouts,.01, .145,STRTRIM(string(e_angle_min),2)+'!Eo!N'+' < emission angle (!16i!X) < ' + STRTRIM(string(e_angle_max),2)+'!Eo!N', /data,siz = .9
                                xyouts,.01, .135,STRTRIM(string(phase_angle_min),2)+'!Eo!N'+' < phase angle(!9a!x) < ' + STRTRIM(string(phase_angle_max),2)+'!Eo!N', /data,siz = .9
                                xyouts,.01, .125,STRTRIM(string(data_avRes_min, format = '(F0.2)'),2)+'km'+' < spatial resolution < ' + STRTRIM(string(data_avRes_max, format = '(F0.2)'),2)+'km', /data,siz = .9
                    
                                device,fil='Lommel_Seeliger/Bennu_Lommel_Seeliger_Viewing_geometry_'+wav[k]+'.ps',/color,xs=20,ys=25,xoff=1,yoff=1 ; set .ps file for viewing geometry plots      
                   
                                !p.font=0
                                !p.multi=[0,1,3]
                                plot,[0],[0],xra=[0 ,120],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Phase Angle(!16!9a!x'+'!Eo!N)',ythi=1, yminor=1
                                oplot, phase_angle, IOF_model/IOF_data, psym=8, col = 250
                               
                                m3 = [0,150]
             
                                oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60
              
                               
                                xyouts, 35, 1.4, 'Fit vs. Phase Angle', /data, siz=1.5, col =0
               
                                plot,[0],[0],xra=[0 ,80],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Incidence Angle(!16i!X!Eo!N)',ythi=1, yminor=1
                 
                                oplot, i_angle, IOF_model/IOF_data, psym=8, col = 250
                                oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60
                                xyouts, 18, 1.4, 'Fit vs. Incidence Angle', /data, siz=1.5, col = 0
                                         
                                plot,[0],[0],xra=[0 ,80],xminor = 2 ,xst=.1,xthi=1.2,psym=3,yra=[.5,1.5],yst=1,ytit='(I/F)!DModeled!N'+'/(I/F)!DMeasured!N', charsize = 2.5, xtit='Emission Angle(!16e!X!Eo!N)',ythi=1, yminor=1
               
                                oplot, e_angle, IOF_model/IOF_data, psym=8, col = 250
                                oplot, m3, replicate(1.0, N_ELEMENTS(m3) ), thi=4, linestyle = 0, col = 60
                                xyouts, 19, 1.4, 'Fit vs. Emission Angle', /data, siz=1.5, col =0
                                
                                parameters = [chisq_m,p[0], p[1], p[2], p[3] ]         
                                data_LS[k, *] = parameters 
                                          
                           ENDFOR                    
                           Result = DIALOG_MESSAGE( 'Lommel Seeliger models are generated, please check Lommel Seeliger directory ',/center, /information,TITLE='Information!!')              
                           result= write_fit_Lommel_Seeliger(data_LS)            
                    ENDIF   
                    device,/close 
     ENDIF    
END


  
  
  

