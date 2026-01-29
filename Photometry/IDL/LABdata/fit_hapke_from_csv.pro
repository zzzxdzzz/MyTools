pro fit_hapke_from_csv, csvfile
  compile_opt idl2

  ; --- Read CSV lines (skip header + the "incidence/emission/phase" label row) ---
  openr, lun, csvfile, /get_lun
  lines = strarr(0)

  while ~ eof(lun) do begin
    line = ''
    readf, lun, line
    lines = [lines, line]
  endwhile

  free_lun, lun

  if n_elements(lines) lt 3 then message, 'CSV file too short.'

  ; --- Data start from the 3rd line (index 2) ---
  n = n_elements(lines) - 2
  ib = dblarr(n)
  eb = dblarr(n)
  ab = dblarr(n)
  rb = dblarr(n)

  for i=0, n-1 do begin
    parts = strsplit(lines[i+2], ',', /extract)

    ; Expected columns:
    ; 0: incidence, 1: emission, 2: phase, 3: Intensity
    ib[i] = double(parts[0])
    eb[i] = double(parts[1])
    ab[i] = double(parts[2])
    rb[i] = double(parts[3])
  endfor

  ; --- Optional sanity check ---
  print, 'N points: ', n
  print, 'Inc range: ', min(ib), max(ib)
  print, 'Emi range: ', min(eb), max(eb)
  print, 'Phase range: ', min(ab), max(ab)
  print, 'Intensity range: ', min(rb), max(rb)

  ; --- Call your Hapke fitting function ---
  ; limi should be defined by you (example placeholder below)
  ; limi = [[p1min,p1max],[p2min,p2max],...]
  parms = fitr_hapke(ib, eb, ab, rb, limits=limi, /plot)

  print, 'Best-fit parms: ', parms
end
