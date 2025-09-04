pro for67p_plot_info
  file_path = "/Users/ZOU/WORK/idl/cometProj/67p/info/"
  file_names = file_search(file_path, '*.sav')
  name=[]
  s1=[]
  s2=[]
  dis=[]
  fn=[]
  fname=[]
  start=[]
  stopt=[]
  nw=[]
  m00=[]
  for i = 0, n_elements(file_names)-1 do begin
     restore, file_names[i]
     name=[name, FILE_NAME]
     s1=[s1,SIZE1]
     s2=[s2,SIZE2]
     dis=[dis, SPACECRAFT_ALTITUDE]
     fn=[fn, FILTER_NUMBER]
     fname=[fname,FILTER_NAME]
     start=[start,START_TIME]
     stopt=[stopt,STOP_TIME]
     r = strsplit(file_names[i],'-',/extract)
     nacwac = REPLICATE(r[2], n_elements(FILE_NAME))
     mm00 = replicate(r[6], n_elements(FILE_NAME))
    nw=[nw,nacwac]
    m00=[m00,mm00]
  endfor
  savename=file_path+"altitude.csv"
  savename1=file_path+'in.csv'
  write_csv, savename, name, s1, s2, dis, fn, fname, start, stopt
  write_csv, savename1, nw,m00
end
