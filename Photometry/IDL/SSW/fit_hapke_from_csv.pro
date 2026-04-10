pro fit_hapke_from_csv
  
csvfile='iear.csv'
  ; --- Read CSV lines (skip header + the "incidence/emission/phase" label row) ---
  ; Load the data (skipping the first 2 header lines)
  data_struct = READ_CSV(csvfile, HEADER=1)

  ; Access your columns (IDL uses FIELD1, FIELD2, etc., by default)
  ib = data_struct.field1
  eb = data_struct.field2
  ab = data_struct.field3
  rb = data_struct.field4

  ; --- Optional sanity check ---
  print, 'N points: ', N_ELEMENTS(ib)
  print, 'Inc range: ', min(ib), max(ib)
  print, 'Emi range: ', min(eb), max(eb)
  print, 'Phase range: ', min(ab), max(ab)
  print, 'Intensity range: ', min(rb), max(rb)

  ; --- Call your Hapke fitting function ---
  ; limi should be defined by you (example placeholder below)
  ; limi = [[p1min,p1max],[p2min,p2max],...]
  limi = [[0.d, 1.d, 0.01d, -0.48d, 0.d], [1.d, 1.d, 0.01d, -0.48d, 60.d]]
  parms = fitr_hapke(ib, eb, ab, rb, limits=limi, /plot)

  print, 'Best-fit parms: ', parms
end
