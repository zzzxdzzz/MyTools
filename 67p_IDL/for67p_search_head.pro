pro for67p_search_head
  M00=["ro-c-osinac-3-prl-67pchuryumov-m06-v2.0",  $
"ro-c-osinac-3-prl-67pchuryumov-m07-v2.0", $
"ro-c-osinac-3-prl-67pchuryumov-m08-v2.0", $
"ro-c-osinac-3-prl-67pchuryumov-m09-v2.0"];, $
;"ro-c-osinac-3-esc1-67pchuryumov-m10-v2.0", $
;"ro-c-osinac-3-esc1-67pchuryumov-m11-v2.0", $
;"ro-c-osinac-3-esc1-67pchuryumov-m12-v2.0", $
;"ro-c-osinac-3-esc1-67pchuryumov-m13-v2.0", $
;"ro-c-osinac-3-esc2-67pchuryumov-m14-v2.0", $
;"ro-c-osinac-3-esc2-67pchuryumov-m15-v2.0", $
;"ro-c-osinac-3-esc2-67pchuryumov-m16-v2.0", $
;"ro-c-osinac-3-esc2-67pchuryumov-m17-v2.0", $
;"ro-c-osinac-3-esc3-67pchuryumov-m18-v2.0", $
;"ro-c-osinac-3-esc3-67pchuryumov-m19-v2.0", $
;"ro-c-osinac-3-esc3-67pchuryumov-m20-v1.0", $
;"ro-c-osinac-3-esc3-67pchuryumov-m21-v1.0", $
;"ro-c-osinac-3-esc4-67pchuryumov-m22-v1.0", $
;"ro-c-osinac-3-esc4-67pchuryumov-m23-v1.0", $
;"ro-c-osinac-3-esc4-67pchuryumov-m24-v1.0", $
;"ro-c-osinac-3-ext1-67pchuryumov-m25-v1.0", $
;"ro-c-osiwac-3-prl-67pchuryumov-m07-v2.0", $
;"ro-c-osiwac-3-prl-67pchuryumov-m08-v2.0", $
;"ro-c-osiwac-3-prl-67pchuryumov-m09-v2.0", $
;"ro-c-osiwac-3-esc1-67pchuryumov-m10-v2.0", $
;"ro-c-osiwac-3-esc1-67pchuryumov-m11-v2.0", $
;"ro-c-osiwac-3-esc1-67pchuryumov-m12-v2.0", $
;"ro-c-osiwac-3-esc1-67pchuryumov-m13-v2.0", $
;"ro-c-osiwac-3-esc2-67pchuryumov-m14-v2.0", $
;"ro-c-osiwac-3-esc2-67pchuryumov-m15-v2.0", $
;"ro-c-osiwac-3-esc2-67pchuryumov-m16-v2.0", $
;"ro-c-osiwac-3-esc2-67pchuryumov-m17-v2.0", $
;"ro-c-osiwac-3-esc3-67pchuryumov-m18-v2.0", $
;"ro-c-osiwac-3-esc3-67pchuryumov-m19-v2.0", $
;"ro-c-osiwac-3-esc3-67pchuryumov-m20-v1.0", $
;"ro-c-osiwac-3-esc3-67pchuryumov-m21-v1.0", $
;"ro-c-osiwac-3-esc4-67pchuryumov-m22-v1.0", $
;"ro-c-osiwac-3-esc4-67pchuryumov-m23-v1.0", $
;"ro-c-osiwac-3-esc4-67pchuryumov-m24-v1.0", $
;"ro-c-osiwac-3-ext1-67pchuryumov-m25-v1.0"]
  
for m = 0,3  do begin
file_path = "/Volumes/PhotoDrive/bak-67pNACWAC/"+M00[m]+"/data/"
 ; file_path = "/Users/ZOU/WORK/idl/cometProj/67p/sample"
 ; save_file = "/Users/ZOU/WORK/idl/cometProj/67p/sample/dataInfo_67P.sav"
save_file = "/Users/ZOU/WORK/idl/cometProj/67p/info-m06-09/"+M00[m]+"-Info.sav"
file_names = file_search(file_path,'*.img')
FILE_NAME=[]
IMAGE_ID=[]
START_TIME=[]
STOP_TIME=[]
TARGET_NAME=[]
SPACECRAFT_SOLAR_DISTANCE=[]
SPACECRAFT_ALTITUDE=[]
PHASE_ANGLE=[]
FILTER_NUMBER=[]
FILTER_NAME=[]
size1=[]
size2=[]

for i = 0, n_elements(file_names)-1 do begin
     img= readpds(file_names[i], headpds=head,/SILENT)
     headlen=n_elements(head)
     k01 = stregex(head, '^FILE_NAME', /boolean) ;19
     k02 = stregex(head, '^IMAGE_ID', /boolean)
     k03 = stregex(head, '^START_TIME', /boolean)
     k04 = stregex(head, '^STOP_TIME', /boolean)
     k05 = stregex(head, '^TARGET_NAME', /boolean)
     k06 = stregex(head, '^SPACECRAFT_SOLAR_DISTANCE', /boolean)
     k07 = stregex(head, '^SPACECRAFT_ALTITUDE', /boolean)
     k08 = stregex(head, '^PHASE_ANGLE', /boolean)
     k09 = stregex(head, '^    FILTER_NUMBER', /boolean)
     k10 = stregex(head, '^    FILTER_NAME', /boolean)
         
     for j = 0, headlen-1 do begin
        if k01[j] then begin           
           result = head[j+1]
           result = strsplit(result, '"', /extract)
           FILE_NAME = [FILE_NAME, result[1]]
           print, result[1]
       endif
       if k02[j] then begin           
           result = head[j]
           result = strsplit(result, /extract)
           IMAGE_ID = [IMAGE_ID, result[2]]
           print, result[2]
        endif
       if k03[j] then begin           
           result = head[j]
           result = strsplit(result, /extract)
          START_TIME = [START_TIME, result[2]]
          print, result[2]

        endif
       if k04[j] then begin           
           result = head[j]
           result = strsplit(result, /extract)
           STOP_TIME = [STOP_TIME, result[2]]
           print, result[2]
        endif
       if k05[j] then begin           
           result = head[j]
           result = strsplit(result, '"', /extract)
           TARGET_NAME = [TARGET_NAME, result[1]]
           print, result[1]
           
        endif
                            
        if k06[j] then begin        
           result = head[j]          
           result = strsplit(result, '=',  /extract)
           result = strsplit(result[1], '<', /extract)
           SPACECRAFT_SOLAR_DISTANCE = [SPACECRAFT_SOLAR_DISTANCE, result[0]]
           print, SPACECRAFT_SOLAR_DISTANCE
        endif
        
        if k07[j] then begin           
           result = head[j]          
           result = strsplit(result, '=',  /extract)
           result = strsplit(result[1], '<', /extract)
           SPACECRAFT_ALTITUDE = [SPACECRAFT_ALTITUDE, result[0]]
           print, SPACECRAFT_ALTITUDE
           ;stop
        endif
        
        if k08[j] then begin           
           result = head[j]          
           result = strsplit(result, '=',  /extract)
           result = strsplit(result[1], '<', /extract)
           PHASE_ANGLE = [PHASE_ANGLE, result[0]]
           print, result[0]
        endif
        if k09[j] then begin           
           result = head[j]          
           result = strsplit(result, '"',  /extract)        
           FILTER_NUMBER = [FILTER_NUMBER, result[1]]
           print, result[1]
        endif
        if k10[j] then begin           
           result = head[j]          
           result = strsplit(result, '"',  /extract)
        
           FILTER_NAME = [FILTER_NAME, result[1]]
           print, result[1]
        endif
      endfor

     if tag_exist(img, 'images') then begin
        si1= size(img.images.image)
     ENDIF ELSE BEGIN
        si1 = size(img.image)
     endelse     
     s1=si1[1]
     s2=si1[2]
        size1=[size1,s1]
        size2=[size2, s2]             
     endfor


save, FILE_NAME, IMAGE_ID,START_TIME,STOP_TIME,TARGET_NAME,SPACECRAFT_SOLAR_DISTANCE,SPACECRAFT_ALTITUDE,PHASE_ANGLE,FILTER_NUMBER,FILTER_NAME, size1, size2, filename=save_file

endfor
end
