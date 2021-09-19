;; apply-generic is called twice. The first dispatch is magnitude of complex package;
;; the second one is is magnitude of rectangula.

(magnitude z)
(apply-generic 'magnitude z)
((get 'magnitude 'complex))
(apply-generic 'magnitude ('rectangular '(3 4)))
((get 'magnitude 'rectangular))
(magnitude '(3 4))



