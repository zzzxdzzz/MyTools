; IDL script to simulate images
;
; 9/19/2017, JYL @PSI
;    based on similar program ../2016-02-PDSSBN/simu_osiris_img.pro

workdir = '/Users/jyli/Work/Reviews/2017-10-PDSSBN/'
aspdir = workdir+'/aspect/'
geodir = workdir+'/calculated1/'
shapefile = workdir+'cg_spc_shap5_050k_cart.wrl'

imfiles = workdir+'Data/'+[ $
    'ro-c-osinac-3-esc1-67pchuryumov-m10-v2.0/data/2014_12/n20141202t103905488id30f22',  $
    'ro-c-osinac-3-esc3-67pchuryumov-m19-v2.0/data/2015_08/n20150801t063848561id30f22',  $
    'ro-c-osinac-3-esc3-67pchuryumov-m20-v1.0/data/2015_08/n20150830t100856344id30f16',  $
    'ro-c-osinac-3-ext1-67pchuryumov-m25-v1.0/data/2016_01/n20160116t222231503id30f22',  $
    'ro-c-osiwac-3-esc1-67pchuryumov-m10-v2.0/data/2014_12/w20141202t103902881id30f18',  $
    'ro-c-osiwac-3-esc3-67pchuryumov-m19-v2.0/data/2015_08/w20150801t063848896id30f18',  $
    'ro-c-osiwac-3-esc3-67pchuryumov-m20-v1.0/data/2015_08/w20150830t100856263id30f18',  $
    'ro-c-osiwac-3-ext1-67pchuryumov-m25-v1.0/data/2016_01/w20160116t220739294id30f12']  $
    +'.img'

inst = [replicate('NAC',4), replicate('WAC',4)]

nacscl = 18.6e-6  ; rad/pixel for NAC
wacscl = 101e-6   ; rad/pixel for WAC
pole = [69.54, 64.11]

; load spice kernels
cspice_furnsh, workdir+'SPICE/ROS_V05_140101_160301.TM'
cspice_furnsh, '/Users/jyli/Work/NAIF/generic_kernels/names.ker'
sc = -226

; load shape model
; readshape_triplate,'/Volumes/LaCie/work/Rosetta/Shape/cg_spc_shap2_024k_cart.wrl',vert,tri
readshape_triplate,workdir+'cg_spc_shap5_788k_cart.wrl',vert,tri

for i=0,n_elements(imfiles)-1 do begin

  print, imfiles[i]

  ; display original image
  im = readpds(imfiles[i])
  im=im.images.image
  name = strmid(imfiles[i],strpos(imfiles[i],'/',/reverse_search)+1)
  name = strmid(name,0,strpos(name,'.'))
  write_png, workdir+name+'.png', bytscl(im)

  lbl = headpds(imfiles[i])
  tmstr = ['SPACECRAFT_CLOCK_START_COUNT*','SPACECRAFT_CLOCK_STOP_COUNT*']
  ;et = dblarr(2)
  ;for j=0,1 do begin
    str = lbl[where(strmatch(lbl, tmstr[0]) eq 1)]
    tm = strmid(str,strpos(str,'=')+1)
    tm = strmid(tm,strpos(tm,'"')+1)
    tm = strmid(tm,0,strpos(tm,'"'))
    cspice_scs2e, sc, tm, e
    ;et[j] = e
  ;endfor
  ;et = avg(et)
  et = e
  cspice_et2utc, et, 'isoc', 3, ut
  cord = subcoord(ut, '67P', observer='Rosetta', range=delta, rh=sundist, pole=pole)
  delta = delta[0]*1.496e8
  sundist = sundist[0] * 1.496e8
  sclon = cord[0]
  sclat = cord[1]
  sslon = cord[2]
  sslat = cord[3]
  noraz = cord[4]   ; clock angle of north pole from sky north

  ; image orientation
  cspice_pxform, 'J2000', 'ROS_OSIRIS_'+inst[i], et, mwac
  ; cspice_pxform, 'J2000', 'ROS_OSIRIS_NAC', et, mnac
  cspice_mxv, mwac, [0., 0., 1.], v
  rd = xyz2rd(v)
  ori = rd[0]   ; clock angle of north in image (from up towards right)

  ; boresight offset from nucleus center
  cspice_spkpos, '67P/C-G',et,'ROS_OSIRIS_'+inst[i],'LT+S','Rosetta', pos, ltime

  if inst[i] eq 'WAC' then pxlscl=wacscl else pxlscl=nacscl
  ct = (pos/delta/pxlscl)[0:1]+[1023.5,1023.5]
  asti_geomap, vert,tri,delta,sclat,sclon,sundist,sslat,sslon,pxlscl, imap, emap, amap, mask, noraz=noraz+180, ori=ori, isz=2048,xc=ct[0],yc=ct[1]
  simu = asti_geo2img(imap, emap, amap, mask, [0.04, -0.4, 1.0, 0.01, 20], /hapke)
  write_png, workdir+name+'_simu.png', bytscl(simu)

endfor

end
