pro hapke_fit_quick, csvfile
  compile_opt idl2

  ; --- Basic file check ---
  if file_test(csvfile, /regular) eq 0 then message, 'CSV not found: ' + csvfile

  ; --- Read all lines robustly (pre-allocate) ---
  nlines = file_lines(csvfile)
  if nlines le 0 then message, 'CSV appears empty: ' + csvfile

  lines = strarr(nlines)
  openr, lun, csvfile, /get_lun
  for i=0, nlines-1 do begin
    line = ''
    readf, lun, line
    lines[i] = strtrim(line, 2)
  endfor
  free_lun, lun

  ; --- Parse numeric rows: incidence, emission, phase, Intensity ---
  ; Use LIST to avoid pre-guessing how many valid rows there are
  ibL = list()
  ebL = list()
  abL = list()
  rbL = list()

  for i=0, nlines-1 do begin
    ; Skip blank lines
    if strlen(lines[i]) eq 0 then continue

    parts = strsplit(lines[i], ',', /extract)
    ; Need at least 4 columns
    if n_elements(parts) lt 4 then continue

    ; Skip header-like rows (non-numeric first column)
    x0 = double(parts[0], /convert_all)
    if finite(x0) eq 0 then continue

    x1 = double(parts[1], /convert_all)
    x2 = double(parts[2], /convert_all)
    x3 = double(parts[3], /convert_all)
    if (finite(x1) eq 0) or (finite(x2) eq 0) or (finite(x3) eq 0) then continue

    ibL.add, x0
    ebL.add, x1
    abL.add, x2
    rbL.add, x3
  endfor

  n = ibL.count()
  if n le 0 then message, 'No numeric data rows parsed from CSV: ' + csvfile

  ; --- Convert LIST -> arrays ---
  ib = dblarr(n)
  eb = dblarr(n)
  ab = dblarr(n)
  rb = dblarr(n)

  for i=0, n-1 do begin
    ib[i] = ibL[i]
    eb[i] = ebL[i]
    ab[i] = abL[i]
    rb[i] = rbL[i]
  endfor

  ; --- Quick sanity print ---
  print, 'N points: ', n
  print, 'Inc range: ', min(ib), max(ib)
  print, 'Emi range: ', min(eb), max(eb)
  print, 'Phase range: ', min(ab), max(ab)
  print, 'Intensity range: ', min(rb), max(rb)

  ; --- Parameter limits: 2x5 (row0=min, row1=max) ---
  limi = [[0.d, 1.d, 0.01d, -0.48d, 0.d], $
          [1.d, 1.d, 0.01d, -0.48d, 60.d]]

  ; --- Fit Hapke model ---
  parms = fitr_hapke(ib, eb, ab, rb, limits=limi, /plot)

  ; --- Output ---
  print, parms
end