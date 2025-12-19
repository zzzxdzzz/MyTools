;+
; NAME:
;
;  load_conf.pro
;
; PURPOSE:
;
;  Load configuration parameters
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  configuration = load_conf(filename[, input=input])
;
; INPUTS:
;
;  filename   - A string to specify the configuration file.
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  input      - A structure to override the parameters of the same
;               names from the configuration file
;
; OUTPUTS:
;
;  Returns a structure that has the configuration parameters
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
; PROCEDURE USED:
;
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : June 26, 2016, created by JYL @PSI
;  v1.1 : July 20, 2016, modified by JYL @PSI
;     Values of configuration is now case sensitive
;  April 7, 2017, JYL @PSI
;    Check existence of configuration file
;  December 14, 2017, JYL @PSI
;    Add capability to accomodate array in key values
;-

FUNCTION load_conf, conf, input=input

    version = '1.0'
    IF NOT keyword_set(input) THEN input = create_struct('inputs', '')
    input_keys = tag_names(input)
    struct = {}

    ; load configuration keywords and values
    IF NOT file_test(conf) THEN message, 'Configuration file '+sisfile+' not found'
    readcol, conf, k, v, delimiter='=', format='a,a', comment='#', /silent
    k = strlowcase(strtrim(k, 2))
    v = strtrim(v, 2)
    nk = n_elements(k)

    ; process keywords
    no_array_keys = ['date', 'name', 'quiet', 'benchmark']
    FOR i=0, nk-1 DO BEGIN
        ; don't check arrays
        IF total(strmatch(no_array_keys, strlowcase(k[i]))) NE 0 THEN vs = v[i] $
        ELSE vs = strtrim(strsplit(v[i],',',/extract),2)
        IF strnumber(vs) THEN vs = float(vs)
        IF n_elements(vs) EQ 1 THEN BEGIN
            vs = vs[0]
            IF typename(vs) EQ 'STRING' THEN BEGIN
                IF vs EQ 'yes' THEN vs = 1  $
                ELSE IF vs EQ 'no' THEN vs = 0
            ENDIF
        ENDIF
        struct = create_struct(struct, k[i], vs)
    ENDFOR
    config_keys = tag_names(struct)

    ; check keywords that needs to be overriden
    input_keys = tag_names(input)
    FOR i=0, n_elements(input_keys)-1 DO BEGIN
      ww = (where(config_keys EQ input_keys[i]))[0]
      IF ww NE -1 THEN struct.(ww) = input.(i)
    ENDFOR

    RETURN, struct

END
