;
; NAME:
; STEP1_DI
;
; PURPOSE:
;    to get all the lon lat imap emap amap and pltmap from Deep Impact
;    data we need to do 2 steps:
;    step1: save fake data and iof data for calculate the delta x and
;    y
;    step2: get all map with x,y and data selection
;
; EXPLANATION:
;    read fits head, read data from JYLi's spice cal results,
;    get the new fits results with all the lon, lat, i, e, a, plt, r
;    using his idl .pro
;
; MODIFICATION HISTORY:
;       Original Version written in 12.7.2015
;       Change to fit for all di data 01.04.2016.

pro step1_di_h2hriv
;  vert   - Vertice list of triangular shape model
;  tri    - Connectivity array of triangular shape model
  readshape_triplate, '/Users/ZOU/DATA/ShapeModels/hartley_8012011.plt', vert, tri ;shape model- 1.0;;;;;;;;;;
  ;readshape_triplate, '/Users/ZOU/DATA/ShapeModels/comet/hartley2_2012_cart.plt', vert, tri ;;2.0
  
  
  ;dir to where the pds .fit
  file_path = "/Users/ZOU/DATA/CometIMG/Hartley2/Hartley2-DIXI-hriv/";????????????
  ;save_path = "/Users/ZOU/DATA/CometIMG/Hartley2/Hartley2-DIXI-hriv_save/";????????????
  save_path1 = "/Users/ZOU/DATA/CometIMG/Hartley2/Hartley2-DIXI-hriv_csv/";????????????
; 
  file_names = file_search(file_path,'*.fit')
  file_size = size(file_names)  
  file_num = file_size[1]       

;;;;;;;;;;;;;;important parameters form JYLi
  data=read_csv('/Users/ZOU/DATA/CometIMG/Hartley2/hartley2-dixi-hriv.txt_coord_new.csv');;????????????
  selat_arr = data.field08
  selon_arr = data.field09

  sslat_arr = data.field10
  sslon_arr = data.field11
  noraz_arr = data.field12 * (-1)   ;polePA8(-1)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for i = 19,19 do begin ;4052
;for i = 0,file_num-1 do begin        
  
    img = readfits(file_names[i], head, h) ;  i
    print, 'filename = ', file_names[i]
      headsize = size(head)
     headlen = headsize[1]
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;           Search for key words                  ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 k_filter = stregex(head, '^FILTERCW', /boolean)
     for j = 0, headlen-1 do begin
        if k_filter[j] then begin           
;  filter  -[nm] Center wavelength for FILTER      
           filter = head[j]          
           filter = strsplit(filter, /extract)           
           filter = double(filter[1]) ;550
           print, 'filter = FILTERCW', filter
        endif
     endfor


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     k_ctrdist = stregex(head, '^CTRDIST', /boolean)
     for j = 0, headlen-1 do begin
        if k_ctrdist[j] then begin           
;  range  - Observer range in km           
           range = head[j]          
           range = strsplit(range, /extract)           
           range = double(range[2]) ;5034.15
           print, 'range = CTRDIST', range
        endif
     endfor
     
     k_tarsunr = stregex(head, '^TARSUNR ', /boolean)
     for j = 0, headlen-1 do begin
        if k_tarsunr[j] then begin           
;  rh     - Heliocentric distance in km
           rh = head[j]           
           rh = strsplit(rh, /extract)           
           rh = double(rh[2])   ;225325075.38
           print, 'rh = TARSUNR', rh
        endif
     endfor     

     k_nompxlsz = stregex(head, '^NOMPXLSZ', /boolean)
     for j = 0, headlen-1 do begin
        if k_nompxlsz[j] then begin           
;  pxlscl - Pixel scale of camera in rad
           pxlscl = head[j]
           pxlscl = strsplit(pxlscl, /extract)
           pxlscl = double(pxlscl[1]) ;1e-5
           print, 'pxlscl = NOMPXLSZ', pxlscl
        endif
     endfor           
     
     k_celestn = stregex(head, '^CELESTN', /boolean)
     for j = 0, headlen-1 do begin
        if k_celestn[j] then begin           
;  ori    - Sky north orientation in image, in deg measured from image
;           y-axis (up direction) clockwise.  This is the same as
;           keyword ORIENTAT used in HST WFPC2 and WFC3 images.
;           Default is 0.
           ori1 = head[j]
           ori1 = strsplit(ori1, /extract)
           ori1 = double(ori1[2]) ;73.37
           print, 'ori = CELESTN', ori1
        endif
     endfor          

     k_nax = stregex(head, '^NAXIS1', /boolean)
     for j = 0, headlen-1 do begin
        if k_nax[j] then begin           
;isz map size
           mapsize = head[j]
           mapsize = strsplit(mapsize, /extract)
           mapsize = long(mapsize[2])
           print, 'mapsize = NAXIS1 ', mapsize
     ;print, 'mapsize = ',mapsize       
        endif
     endfor 

     k_mult = stregex(head, '^MULT2IOF', /boolean)
     for j = 0, headlen-1 do begin
        if k_mult[j] then begin           
;img data to I/F ;mult2iof
     mult2iof = head[j]
     mult2iof = strsplit(mult2iof, /extract)
     mult2iof = double(mult2iof[1])
     print, 'mult2iof = MULT2IOF',mult2iof
        endif
     endfor 
  
;  noraz  - Azimuth angle of north pole, in deg clockwise from sky
;           north.  This is the same definition as returned by
;           subcoord.pro.  Default is 0.
     polePA = noraz_arr[i]
     print, 'polePA=', polePA
;  sslat  - Sub-solar latitude in deg
     sslat = sslat_arr[i]
     print, 'sslat=',sslat
;  sslon  - Sub-solar longitude in deg
     sslon = sslon_arr[i]
     print, 'sslon=',sslon
;  selat  - Sub-observer latitude in deg
     selat = selat_arr[i]
     print, 'selat=', selat
;  selon  - Sub-observer longitude in deg
     selon = selon_arr[i]
     print, 'selon=', selon
     
;;;;;;;;;;;;;;;core;;;;;;;;;;;;;;;;;;
     asti_geomap, vert, tri, range,selat,selon, rh, sslat,sslon, pxlscl,imap, emap, amap, mask,pltmap, noraz = polePA, ori=ori1, isz=mapsize
    ; asti_geomap, vert, tri, range,selat,selon, rh, sslat,sslon, 1e-5,imap, emap, amap, mask,pltmap, noraz = polePA, ori=ori1, isz=mapsize    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
;img data to I/F
;mult2iof;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;last images are short headered     
     img_iof = img * mult2iof
  window, xs=mapsize, ys=mapsize ;show the image
  tvscl, imap
  image=asti_geo2img(imap,emap,amap,mask,/lambert) ;compare to image
  tvscl,image

; tvscl, img_iof

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SAVE

   ii=string(i, format='(I08)')
   fi = strjoin([save_path1,ii,'fake','.csv'])
   
   WRITE_CSV, Fi, image
   
   fil = strjoin([save_path1,ii,'iof','.csv'])
   
   WRITE_CSV, Fil, img_iof
      
    ; savethis = strjoin([save_path,ii, '.sav'])
    ; name = file_names[i]
    ; save, name, filter, img_iof, imap, emap, amap, mask, pltmap, filename=savethis
     
  endfor
end
