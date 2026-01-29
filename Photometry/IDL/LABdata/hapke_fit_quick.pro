function str2d_safe, s
  compile_opt idl2

  ; --- Safely convert string to DOUBLE; return NaN on failure ---
  catch, err
  if err ne 0 then begin
    catch, /cancel
    return, !values.d_nan
  endif

  v = double(strtrim(s, 2))
  catch, /cancel
  return, v
end


pro hapke_fit_quick, csvfile
  compile_opt idl2

  ; --- Basic file check ---
  if file_test(csvfile, /regular) eq 0 then message, 'CSV not found: ' + csvfile

  ; --- Containers for numeric columns ---
  ibL = list() & ebL = list() & abL = list() & rbL = list()

  ; --- Stream-read CSV and parse numeric rows ---
  openr, lun, csvfile, /get_lun
  while ~eof(lun) do begin
    line = ''
    readf, lun, line
    line = strtrim(line, 2)

    ; Skip blank lines
    if strlen(line) eq 0 then continue

    parts = strsplit(line, ',', /extract)
    if n_elements(parts) lt 4 then continue

    x0 = str2d_safe(parts[0])
    x1 = str2d_safe(parts[1])
    x2 = str2d_safe(parts[2])
    x3 = str2d_safe(parts[3])

    ; Skip non-numeric/header/bad rows
    if (finite(x0) eq 0) or (finite(x1) eq 0) or (finite(x2) eq 0) or (finite(x3) eq 0) then continue

    ibL.add, x0 & ebL.add, x1 & abL.add, x2 & rbL.add, x3
  endwhile
  free_lun, lun

  n = ibL.count()
  if n le 0 then message, 'No numeric data rows parsed from CSV: ' + csvfile

  ; --- Convert LIST -> arrays ---
  ib = dblarr(n) & eb = dblarr(n) & ab = dblarr(n) & rb = dblarr(n)
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

  ; --- Fit Hapke model (disable plotting to avoid PLOTHIST crash) ---
  parms = fitr_hapke(ib, eb, ab, rb, limits=limi)

  print, parms
end
