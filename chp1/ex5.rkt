;ex1.5

; 1. For LCExp with only identifier, there are no parenthesis, trivially balanced parens.
; 2. For (lambda (ident) LcExp there is n + 2 sets of parens, if LcExp has n.
; 3. For LCExp in the case of higher order LCEexp (LCExp LCExp) we have n and m sets of parens + .
