#lang racket

(require odysseus)
(require "vk.rkt")

(provide (all-defined-out))

; uname - Сергей_Шегурин
; uurl - vk.com/shegurin
; ualias - shegurin
; uid - 3614110
; uurlid - vk.com/id3614110
; user - (hash 'uname "Сергей_Шегурин" 'ualias "shegurin" 'uid "3614110" 'f "8923")

(define USER-SKEL (hash 'uname #f 'ualias #f 'uid #f 'f #f))

(define (get-uurlid uid)
	(format "vk.com/id~a" uid))

(define-catch (get-user uid #:cache (cache #f) #:p (p #f))
	(let ((user-from-cache (and cache (hash-ref cache uid #f))))
		(or user-from-cache
				(let* (
							(user (get-user-info uid #:p p))
							(user (or user (hash)))
							(err (not (or ($ first_name user) ($ last_name user))))
							(uname (format "~a_~a"
															(or ($ first_name user) "")
															(or ($ last_name user) "")))
							(ualias ($ domain user))
							(friends-count ($ counters.friends user)))
					(cond
						(err #f)
						(else
							(hash
								'uname uname
								'ualias ualias
								'uid uid
								'f friends-count)))))))
