;EX 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 1. S = {3n + 2 | n in N}
;; Top Down
;; 1. x is 2
;; 2. x - 3 in S
;;
;; Bottom Up
;; 1. 2 in S
;; 2. if x in S, then n + 3 in S
;;
;; Rules of Inference
;; 2 in S
;; x in S / x + 3 in S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2. S = {2n + 3m + 1 | n, m in N}
;; x in s iff
;; Top Down
;; 1. x is 1
;; 2. x - 2 in S
;; 3. x - 3 in S
;; Bottom Up
;; 1. 1 in S
;; 2. if x in S, then x + 2 in S
;; 3. if x in S, then x + 3 in S
;; Rules of Inference
;; 1 in s
;; x in S / ((x + 2 in S) (x + 3 in S))

;; 3. S = {(n, 2n + 1) | n in N}
;; (n, m) is in S iff
;; Top Down
;; 1. (n, m) is (0, 0)
;; 2. (n - 1, m - 2) in S
;; Bottom Up
;; 1. (0, 0) in S
;; 2. (n, m) in S, then (n + 1, m + 1) in S
;; Rules of Inference
;; (0, 0) in S
;; (n, m) in S / (n + 1, m + 1 ) in S

;; 4. S = {(n, n^2) | n in N}
;; (n, m) is in S iff
;; Top Down
;; 1. (n, m) is (0, 0)
;; 2. (n - 1, m - 2n + 1) in S
;; Bottom Up
;; 1. (0, 0) in S
;; 2. (n, m) in S, then (n + 1, m + 2n + 1) in S
;; Rules of Inference
;; (0, 0) in S
;; (n, m) in S / (n + 1, m + + 2n + 1 ) in S


