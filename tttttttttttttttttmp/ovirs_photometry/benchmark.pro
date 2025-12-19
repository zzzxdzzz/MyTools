;+
; NAME:
;
;  benchmark.pro
;
; PURPOSE:
;
;  Print out benchmark information
;
; CATEGORY:
;
;  Utility
;
; CALLING SEQUENCE:
;
;  t1 = benchmark(t0, task)
;
; INPUTS:
;
;  t0         - Double precision number of system time, usually the output
;               of systime(1)
;  task       - A string for print output
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  Returns the time stamp systime(1) at the time of call this procedure
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
;  v1.0 : July 15, 2016, created by JYL @PSI
;-

FUNCTION benchmark, t0, task

    t1 = systime(1)
    sys_quiet = !quiet
    !quiet = 0
    message, /info, task+' completed in '+string((t1-t0)/60)+' min'
    !quiet = sys_quiet
    RETURN, t1

END
