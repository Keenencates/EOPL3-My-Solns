;ex1.14

; n = 0, function return v(0) which is equal to the sum of v(0:0) = V(0)
; n > 0, Assume sum is correct for n, i.e. v(n). Then fn returns
; v(n) + sum(v(n-1)) -> v(n) + v(n-1) + sum(v(n-2)) -> ... > v(n) + v(n-1) + ... v(0) = sum(0 to n)(v)
