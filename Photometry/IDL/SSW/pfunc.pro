function pfunc, g, alpha, form=form, parameters=params

;+
; NAME:
;   pfunc.pro
; PURPOSE:
;   Calculate the single-particle phase function.
; DESCRIPTION:
;   This program calculate the single-particle phase function using specified 
;   form.  The commonly used forms include: single-term HG, double-term HG, 
;   and polynomial.  It can be specified by keyword "form", and parameters 
;   are passed by another keyword "parameters".
;
;   This program is actually a wrapper to select and call subroutines which do 
;   the real calculations of the particle phase function.  It can be easily 
;   expanded to include more forms in the future.  The new form that will be 
;   added should be specified by an index number so that it can be called by 
;   "form" keyword.  The parameters to that form will be passed by keyword 
;   "parameters".
;
;   To add new forms to the program, user should first write a subroutine that 
;   performs the calculation of new particle phase function.  Then the part 
;   marked by >>>> in this program should be updated accordingly, 
;   following the instructions in the program.
;
;   IMPORTANT: Due to historical reason and backward compatability, the first 
;   argument to this routine is the single parameter for single-term HG form if 
;   it is the form selected, and is a dummy but must be present if other forms 
;   of phase function are specified.  The second argument is the array of phase 
;   angles.  The default form is the single-term HG, if no keyword is specified.  
;   All other forms are specified by keyword "form", and the parameters must be 
;   passed by keyword "parameters".
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   output = pfunc(g, alpha[, form=#][, parameters=array])
;
; INPUTS:
;   g           - Single-term HG parameter, if it it the selected phase 
;                 function.  Or a dummy if other forms of phase functions are 
;                 selected.  It must present.
;   alpha       - An array of phase angles that the phase function is 
;                 calculated.
;
; KEYWORD INPUT PARAMETERS:
;   forms       - A number to select the form of phase function that will be 
;                 used.  If more forms are added later, the following lines 
;                 should be updated accordingly.
;                 1: (default) single-term HG
;                 2: double-term HG
;                 3: polynomial
;   parameters  - An array of floating numbers, the parameters of phase 
;                 function.  It can be omitted only if single-term HG is 
;                 specified.  If it is present for single-term HG, it will 
;                 override the g argument passed directly to the program.
;
; OUTPUTS:
;   Program returns an array of phase function calculated for specified form and 
;   input parameters under the input phase angles.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
; SIDE EFFECTS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   9/27/2007, created by JYL @UMd
;
;-


on_error, 2


if n_params() lt 1 or n_params() gt 2 then begin
  print, 'Usage: output = pfunc(g, alpha[, form=#][, parameters=array])'
  return, 0.
endif


if not keyword_set(form) then form=1
if not keyword_set(params) then params=[g]

case form of

  1: return, hg_func(alpha, params[0])
  2: return, hg_func(alpha, params[0], params[1], params[2])
  3: return, poly_phase(alpha, params[0], params[1])
  else: begin
    print, 'Specified single-particle phase function is not recoganized!'
    return, 0.
  end
  
endcase

end
