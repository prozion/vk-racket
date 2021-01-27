#lang racket

(require odysseus)
(require odysseus/read/vk)

(provide (all-defined-out))

; gname - Технологии_долголетия
; fgurl - https://vk.com/longtech
; gurl - vk.com/longtech
; galias - longtech
; gid - 104545950
; gurlid - vk.com/club104545950
; group - (hash 'gname "Технологии_долголетия" 'galias "longtech" 'gid "104545950" 'u "95713" '_parent "лонжевити_и_трансгуманизм")

(define GROUP-SKEL (hash 'gname #f 'galias #f 'gid #f 'u #f '_parent #f))

(define (get-gurlid gid)
	(format "vk.com/club~a" gid))

(define (get-users-number group)
	(and
		group
		(or ($ u group) ($ p group) ($ f group) #f)))

(define-catch (make-group-from-item item)
	(hash-union
		(hash 'gname ($ id item)
					'galias (and ($ vk item) (extract-pure-alias ($ vk item)))
					'u ($ u item)
					'_parent ($ _parent item))
		GROUP-SKEL
		))

(define-catch (add-gid-to-group group #:cache (cache #f) #:p (p #f))
	(let* ((group-in-the-cache (and cache (hash-ref cache ($ gname group) #f)))
				(already-in-cache? (and group-in-the-cache ($ gid group-in-the-cache))))
		(if already-in-cache?
			group-in-the-cache
			(begin
				(if p
					(begin (sleep (first p)) (flush-output) (display (second p)))
					(void))
				(hash-set group 'gid (get-gid ($ galias group)))))))

(define-catch (add-u-to-group group #:cache (cache #f) #:p (p #f))
	(let* ((group-in-the-cache (and cache (hash-ref cache ($ gname group) #f)))
				(cached-users-number (get-users-number group-in-the-cache))
				(already-in-cache? (and cache group-in-the-cache cached-users-number)))
		(if already-in-cache?
				(hash-union (hash 'u cached-users-number) group-in-the-cache)
				(begin
					(if p
						(begin (sleep (first p)) (flush-output) (display (second p)))
						(void))
					(hash-set group 'u (get-group-members-count ($ gid group)))))))
