; docformat = 'rst'

;+
; FITS file comparison utility
;
; :Author:
;    ${user}
;
; :History:
;    ${user}, ${date}: initial template
;
;-


;+
; :Description:
;    Compare two FITS files, print the differences
;
; :Params:
; file1    : required, the first (original) FITS file
; file2    : required, the second (new) FITS file
; exclude  : optional, array of string, keywords to be excluded from
;            comparison, case insensitive
; quiet    : optional, suppress print out
; diff     : optional named variable, returns difference status:
;            diff = 0: identical, diff = 1: different

; In IDL environment, this program is used as normal
; :Example:
; fits_diff, file1.fits, file2.fits
;
; For SPOC interface, these file names will be passed as command line arguments via a system call
; :Example:
; idl -rt=fits_diff.pro.sav -args file1.fits file2.fits
;
; :History:
;     MRF,  12/02/2015  Initial version
;     JYL,  04/20/2017  Add keywords 'exclude', 'diff', 'quiet'
;
;-

function header_to_hash, hdr
  hash = HASH()
  cnt = N_Elements(hdr)
  For i=0, cnt-1 Do Begin
    len = strLen(hdr[i])
    key = strTrim(strmid(hdr[i], 0, 8), 2)
    If (key EQ 'END') Then break
    chk = strmid(hdr[i], 8, 1)
    If (chk EQ '=') Then Begin
      idxq = strPos(hdr[i], "'", 9)
      If (idxq LT 0) Then idxq = len
      idx = strPos(hdr[i], '/', 9)
      If (idx LT 0) Then idx = len
      If (idxq LT idx) Then Begin
        idxq = strPos(hdr[i], "'", idxq+1)
        If (idxq LT 0) Then idxq = len
        idx = strPos(hdr[i], '/', idxq+1)
        If (idx LT 0) Then idx = len
      EndIf
      val = strTrim(strmid(hdr[i], 9, idx - 9), 2)
    EndIf Else Begin
      If hash.HasKey(key) Then val = hash[key] + ' ' Else val = ''
      val = val + strTrim(strmid(hdr[i], 9), 2)
    EndElse
    hash[key] = val
  EndFor
  return, hash
end

pro fits_diff, file1, file2, exclude=exclude, diff=diff, quiet=quiet
  ; Report exceptions where/when they occur
  ;!EXCEPT=2

  if not keyword_set(exclude) then exclude=[]
  if not keyword_set(quiet) then quiet=0
  sys_quiet = !quiet
  !quiet = quiet

  ; Retrieve the command line arguments to the pipeline
  args = command_line_args(count=numparam)

  if (numparam ne 0) then begin
    file1 = args[0]
    file2 = args[1]
  endif
  if ~quiet then begin
    print, '< ', file1
    print, '> ', file2
  endif

  diff = 0u
  ext = 0
  While 1 Do Begin
    img1 = mrdfits(file1, ext, hdr1, STATUS=status1, silent=quiet)
    img2 = mrdfits(file2, ext, hdr2, STATUS=status2, silent=quiet)

    If ((status1 NE 0) AND (status2 NE 0)) Then break

    If ((status1 EQ 0) AND (status2 EQ 0)) Then Begin
      hash1 = header_to_hash(hdr1)
      hash2 = header_to_hash(hdr2)
      keys = (hash1 NE hash2).toArray()
      cnt = N_Elements(keys)
      if ~quiet then print, 'HDU:', ext
      If (cnt GT 0) Then Begin
        for i=0, N_Elements(exclude)-1 do begin
          ww = where(strcmp(keys, exclude[i], /fold_case))
          if ww[0] ne -1 then begin
            if N_Elements(keys) EQ 1 then keys=[] else remove, ww[0], keys
          endif
          if N_Elements(keys) EQ 0 then break
        endfor
        cnt = N_Elements(keys)
        sorted = sort(keys)
        If (cnt GT 0) Then Begin
          diff = 1u
          For i=0, cnt-1 Do Begin
            key = keys[sorted[i]]
            If hash1.HasKey(key) Then val1 = hash1[key] Else val1 = 'Missing'
            If hash2.HasKey(key) Then val2 = hash2[key] Else val2 = 'Missing'
            if ~quiet then print, key, ': <', val1, ' != >', val2
          EndFor
        EndIf
      EndIf

      ; Compare type
      type1 = size(img1, /Tname)
      type2 = size(img2, /Tname)
      If (type1 NE type2) Then Begin
        diff = 1u
        if ~quiet then print, 'HDU types differ: ', type1, ' vs ', type2
        continue
      EndIf
      If (type1 EQ 'STRUCT') Then Begin
        cols = N_Tags(img1)
        If (N_Tags(img2) NE cols) Then Begin
          diff = 1u
          if ~quiet then print, 'HDU table columns differ: ', cols, ' vs ', N_Tags(img2)
          continue
        EndIf
        diffcnt = LonArr(cols)
        For i=0, cols-1 Do Begin
          idx = where(img1.(i) NE img2.(i), cnt)
          diffcnt[i] = cnt
        EndFor
        If (Total(diffcnt, /Int) EQ 0L) Then Begin
          if ~quiet then print, 'Identical tables'
        EndIf Else Begin
          diff = 1u
          if ~quiet then print, 'Tables differs, cnts =', diffcnt
        EndElse
      EndIf Else Begin
        idx = where((img1 - img2) NE 0, cnt)
        If (cnt EQ 0) Then Begin
          if ~quiet then print, 'Identical images'
        EndIf Else Begin
          diff = 1u
          if ~quiet then if ~quiet then print, 'Images differ, pixel cnt =', cnt
        EndElse
      EndElse
    EndIf Else If (status1 EQ 0) Then Begin
      diff = 1u
      if ~quiet then print, file2, ' does not contain externsion', ext
    EndIf Else If (status2 EQ 0) Then Begin
      diff = 1u
      if ~quiet then print, file1, ' does not contain externsion', ext
    EndIf
    ext++
  EndWhile

  !quiet = sys_quiet
  return
end
