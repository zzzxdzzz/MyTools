pro inspect_one_sav, savfile
  compile_opt idl2

  ; 清理一下当前 scope（可选）
  ; 读入
  restore, savfile

  ; 列出当前 scope 里有哪些变量
  vars = scope_varfetch(/common)  ; 有些sav会落到common里
  if n_elements(vars) eq 0 then vars = scope_varfetch()

  print, '--- SAV:', savfile
  print, 'Variables:'
  for i=0, n_elements(vars)-1 do begin
    vname = vars[i]
    ; 取变量并打印size/type
    value = scope_varfetch(vname, /no_copy)
    help, value, /structure, /implied_print, name=vname
  endfor
end

