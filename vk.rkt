#lang racket

(require net/url)
(require json)
(require compatibility/defmacro)
(require odysseus)
(require odysseus/json)
(require odysseus/http)
(require odysseus/optimize)

(provide (all-defined-out))

(define status-output (make-parameter #f))
(define friends-limit (make-parameter #f))

; Алгоритм получения access_token ВКонтакте
; -- открываем https://vk.com/apps?act=manage
; -- создаем приложение [выбираем "standalone приложение", вводим имя]
; -- оставляем "приложение отключено"
; -- переходим на вкладку "настройки", записываем id
; -- загружаем ссылку в адресное поле браузера, где вместо [id] вписываем значение id из предыдущего этапа https://oauth.vk.com/authorize?client_id=[id]&display=page&scope=friends,offline,groups,stats,wall,notes,pages,video,audio,photos,stories,status,docs&response_type=token&v=5.103
; -- подтверждаем доступ
; -- возвращается новая адресная строка с параметром access_token. Он нам и нужен, записываем его в укромное место (в данном случае в отдельный файл вне публичного репозитория)

(define VK_API_VERSION "5.107")

(define VK_ERROR_MESSAGES
        (hash
          'THE_PROFILE_IS_BLOCKED_OR_DELETED 18
          'THE_PROFILE_IS_PRIVATE 30
        ))

(define _AT (make-parameter ""))

(define (set-access-token at)
  (_AT at)
  (void))

(define-catch (dump-ids-to-file ids)
  (let ((comma-separated-ids (implode ids ",")))
    (write-file "ids.txt" comma-separated-ids)))

(define-catch (read-ids-from-file filename)
  (let ((ids-string (read-file filename)))
    (string-split ids-string ",")))

;;;;; User ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define special-values (list "<user-deactivated>"))

(define user-basic-fields '(first_name last_name deactivated hidden))
(define user-optional-fields-all '(
                                about activities
                                bdate blacklisted blacklisted_by_me books
                                can_post can_see_all_post can_write_private_message career city common_count connections contacts counters country crop_photo
                                domain
                                education exports
                                first_name_nom first_name_gen first_name_dat first_name_acc first_name_ins first_name_abl
                                followers_count friend_status
                                games
                                has_mobile has_photo home_town
                                interests is_favorite is_friend is_hidden_from_feed
                                last_name_nom last_name_gen last_name_dat last_name_acc last_name_ins last_name_abl
                                last_seen lists
                                maiden_name military movies music
                                nickname
                                occupation online
                                personal
                                photo_id photo_max photo_max_orig photo_50 photo_100 photo_200_orig photo_400_orig
                                quotes
                                relatives relation
                                schools screen_name sex site status
                                timezone trending tv
                                universities
                                verified
                                wall_default
                              ))
(define personal-object-fields '(political langs religion inspired_by people_main life_main smoking alcohol))
(define schools-array-fields '(id country city name year_from year_to year_graduated class speciality type type_str))
(define universities-array-fields '(id country city name faculty faculty_name chair chair_name graduation education_form education_status))

(define user-basic-fields-used '(first_name last_name))
(define user-optional-fields-used
                              '(
                                bdate
                                ; blacklisted blacklisted_by_me
                                books
                                ;can_post can_see_all_post can_write_private_message
                                career city common_count connections contacts counters country
                                ; crop_photo
                                domain
                                education exports
                                ; first_name_nom first_name_gen first_name_dat first_name_acc first_name_ins first_name_abl
                                followers_count friend_status
                                games
                                has_mobile has_photo home_town
                                interests is_favorite is_friend is_hidden_from_feed
                                ; last_name_nom last_name_gen last_name_dat last_name_acc last_name_ins last_name_abl
                                last_seen lists
                                maiden_name military movies music
                                nickname
                                occupation online
                                personal
                                ; photo_id
                                photo_max
                                ; photo_max_orig photo_50 photo_100 photo_200_orig photo_400_orig
                                quotes
                                relatives relation
                                schools screen_name sex site status
                                timezone trending tv
                                universities
                                verified
                                wall_default
                              ))

(define user-fields (append user-basic-fields-used user-optional-fields-used))

(define-catch (get-user-info user-ids #:fields (fields user-fields) #:status (status #f) #:access-token (access-token (_AT)) #:p (p #f))
  (let* (
        (fields (implode fields ","))
        (user-ids (cond
                    ((list? user-ids) (implode user-ids ","))
                    (else user-ids)))
        (_ (print-with-delay p))
        (request (format "https://api.vk.com/method/users.get?user_ids=~a&fields=~a&v=~a&access_token=~a" user-ids fields VK_API_VERSION (_AT)))
        (res (json->hash (get-url request))))
        ; (res (hash)))
    (if (vk-error? res)
        (begin (--- (vk-error-msg res)) #f)
        (car (@. res.response)))))

;; user properties
(define (request-username id (glue " "))
  (when (status-output) (display "+") (flush-output))
  (let ((res (string->jsexpr
                (get-url (format "https://api.vk.com/method/users.get?user_ids=~a&v=~a&access_token=~a" id VK_API_VERSION (_AT))))))
    (if (@. res.error)
      id
      (let ((u (car (@. res.response))))
        (format "~a~a~a"
                  (string-replace (@. u.first_name) " " glue)
                  glue
                  (string-replace (@. u.last_name) " " glue))))))

(define vk/username request-username)

(define (vkfy id)
  (format "vk.com/~a" id))

(define (repost? item)
  (true? ($ copy_history item)))

;; output
(define (vk/id->href id)
  (format "<a href=\"~a\" target=\"_blank\">~a</a>" (vk/link id) (request-username id)))

(define (vk/ids->hrefs paths)
  (when (status-output) (display "\nrequesting names"))
  (let ((m-request-username (memoize request-username))) ; function with static hash through closure
    (map
      (λ (x) (map
                (λ (id) (format "<a href=\"~a\" target=\"_blank\">~a</a>" (vk/link id) (m-request-username id)))
                x))
      paths)))

(define (vk/link id)
  (format "https://vk.com/id~a" id))

(define (get-user-id uurl #:delay (delay-time #f) #:display? (display? #f))
  (let* ((ualias (last (string-split uurl "/")))
        (plain-name? (re-matches? #px"^(id)(\\d+)$" ualias)))
    (if plain-name?
        (let* ((n (get-matches #px"^(id)(\\d+)$" ualias))
              (n (third (first n))))
            (--- 111 ualias)
            n)
        (let* (
              (_ (when delay-time (sleep delay-time)))
              (_ (when display?
                      (display display?)
                      (flush-output)))
              (res-user (string->jsexpr
                          (get-url (format "https://api.vk.com/method/users.get?user_ids=~a&fields=~a&v=~a&access_token=~a" ualias "id" VK_API_VERSION (_AT)))))
              (result-user (and ($ response res-user) (not-empty? ($ response res-user)) ($ id (first ($ response res-user))))))
          (and result-user (->string result-user))))))

(define get-uid get-user-id)

(define-catch (get-friends-of-user user-id #:delay (delay-time #f) #:display? (display? #f) #:friends-limit (friends-limit #f))
  (let* (
        (_ (when delay-time (sleep delay-time)))
        (_ (when display?
                (display display?)
                (flush-output)))
        (res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/friends.get?user_id=~a&v=~a&access_token=~a" user-id VK_API_VERSION (_AT))))))
    (cond
      ((@. res.error)
        empty)
      (else
        (let ((items (@. res.response.items)))
          (if (and friends-limit (> (length items) friends-limit))
            (take items friends-limit)
            items))))))

(define-catch (get-groups-of-user user-id #:delay (delay-time #f) #:display? (display? #f) #:groups-limit (groups-limit #f))
  (let* (
        (_ (when delay-time (sleep delay-time)))
        (_ (when display?
                (display display?)
                (flush-output)))
        (res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/groups.get?user_id=~a&v=~a&access_token=~a" user-id VK_API_VERSION (_AT))))))
    ; (--- res)
    (cond
      ((@. res.error)
        empty)
      (else
        (let ((items (@. res.response.items)))
            (if (and groups-limit (> (length items) groups-limit))
              (take items groups-limit)
              items))))))

(define-catch (output-user-file filename)
  (let ((users (read-serialized-data-from-file filename)))
    (for ((user (hash-values users)))
      (when (and ($ first_name user) ($ last_name user) (not (equal? ($ first_name user) "DELETED")))
        (display (format "~a ~a,~a,~a  ~a~n"
                      (or ($ first_name user) "")
                      (or ($ last_name user) "")
                      (or ($ bdate user) "")
                      (or
                        ($ city.title user) ($ home_town user)
                        "")
                      (let ((status ($ status user)))
                        (if (no-words? status) "" (str "[" (string-replace status "\n" "") "]")))
                      ))))))

(define-catch (number-of-users filename)
  (let ((users (read-serialized-data-from-file filename)))
    (--- (length (hash-keys users)))))

;;; Groups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (remove-vk-url-prefix url)
  (last (string-split url "vk.com/")))

(define-catch (get-group-name groupid)
  (let* ((groupid (remove-vk-url-prefix groupid))
        (res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/groups.getById?group_id=~a&v=~a&access_token=~a" groupid VK_API_VERSION (_AT))))))
    (if (@. res.error)
      null
      (hash-ref (car (hash-ref res 'response)) 'name))))

(define-catch (get-group-members-count groupid)
  (let* ((groupid (remove-vk-url-prefix groupid))
        (res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/groups.getMembers?group_id=~a&v=~a&access_token=~a" groupid VK_API_VERSION (_AT))))))
    (if (@. res.error)
      #f
      (@. res.response.count))))

; I don't define-catch here as it's better to process exceptions in the code upstream, when requesting id in a row among hundreds of urls
(define-catch (get-gid gurl #:cache (cache #f) #:delay (delay-time #f) #:display? (display? #f))
  (define plain-name? (re-matches? #px"^(club|public|id)?(\\d+)$" gurl))
  ; (--- gurl plain-name?)
  (define (get-name-from-full-url group-url)
    (last (string-split group-url "/")))
  (let* ((galias (get-name-from-full-url gurl)))
    (or
      (and cache (hash-ref cache galias #f))
      (if plain-name?
          (let* (
                (n (get-matches #px"^(club|public|id)?(\\d+)$" galias))
                (n (third (first n))))
              n)
          (let* (
                (_ (when delay-time (sleep delay-time)))
                (_ (when display?
                        (display display?)
                        (flush-output)))
                (res-group (string->jsexpr
                                  (get-url (format "https://api.vk.com/method/groups.getById?group_id=~a&v=~a&access_token=~a" galias VK_API_VERSION (_AT)))))
                (result-group (and ($ response res-group) (not-empty? ($ response res-group)) ($ id (first ($ response res-group))))))
            (and result-group (->string result-group)))))))

(define get-group-id get-gid)

(define-catch (get-vk-name-from-url vk-url)
  (let* (
        (vk-name (get-matches #px"^vk\\.com/(.*)$" vk-url))
        (vk-name (and
                      (not-empty? vk-name)
                      (match-let
                          (((list (list _ x)) vk-name))
                        x)))
        (vk-name (and vk-name (not (empty? vk-name)) vk-name)))
    vk-name))

(define (vk/get-photoalbums group-id #:group? (group? #t))
  (let*
      ((res (string->jsexpr
                  (get-url (format "https://api.vk.com/method/photos.getAlbums?owner_id=~a~a&need_system=1&v=~a&access_token=~a"
                                    (if group? "-" "")
                                    group-id
                                    VK_API_VERSION
                                    (_AT)))))
      (res (and ($ response res) ($ items ($ response res)))))
    res))

(define (raw-community? type)
  (λ (groupid)
    (regexp-match (pregexp (str (symbol->string type) "\\d+")) groupid)))

(define raw-group? (raw-community? 'club))
(define raw-public? (raw-community? 'public))
(define raw-event? (raw-community? 'event))
(define raw-person? (raw-community? '__id))

(define-catch (pure-id? vk-url)
  (let* ((vk-url (->string vk-url))
        (vk-url (remove-vk-url-prefix vk-url)))
    (or
      (raw-group? vk-url)
      (raw-public? vk-url)
      (raw-event? vk-url)
      (raw-person? vk-url))))

(define-catch (extract-pure-alias vk-url)
  (let* ((vk-url (->string vk-url))
        (vk-url (remove-vk-url-prefix vk-url)))
    (cond
      ((raw-group? vk-url) (drop* vk-url (string-length "club")))
      ((raw-public? vk-url) (drop* vk-url (string-length "public")))
      ((raw-event? vk-url) (drop* vk-url (string-length "event")))
      ((raw-person? vk-url) (drop* vk-url (string-length "id")))
      (else vk-url))))

; Функция по заданному id группы groupid возвращает список id ее участников.
(define (get-group-users
          groupid ; id группы (это цифры в url группы типа vk.com/club14881917, либо получаемые по алиасу группы через get-gid)
          #:offset (offset 0) ; смещение по выборке, если 0 - то приходит первая тысяча id, если 10000 - то id с позицией от 10001 до 11000 в общем списке участников
          #:count (count 1000)
          #:delay (delay-time #f) ; задержка между последовательными обращениями к серверу ВКонтакте по API, нужна, чтобы не словить ошибку "Too many requests per second"
          #:display? (display? #f) ; отображать каждый запрос по API в консоли, и если да, то какие символы?
          )
  ; Внутренняя рекурсивная функция.
  ; Зачем? - За один запрос по VK API можно получить лишь 1000 id участников.
  ; Бывают группы, где количество участников несколько сотен тысяч и даже миллионы.
  ; Поэтому нужно последовательно делать запросы с использованием параметра offset.
  ; Для этого создаем рекурсивную функцию, которая будет запрашивать следующую и следующую тысячу id пользователей,
  ; пока не уткнется в ошибку или пустой результат.
  (define (get-next-users
              groupid
              offset
              (acc-result empty) ; результат после каждой итерации накапливается в итоговый список
              )
    (when delay-time (sleep delay-time)) ; если задано время задержки, самое время сделать паузу и скушать Твикс
    (when display?
          (display display?)
          (flush-output))
    (let ((res (string->jsexpr ; ответ приходит в виде текстовой строки формата JSON, парсим его в структуру данных типа вложенного хэша
                  (get-url ; эта функция посылает по указанному URL HTTP запрос типа GET
                    (format
                      ; запрос формируем согласно документации https://vk.com/dev/groups.getMembers.
                      ; Этот адрес с подставленными значениями можно прямо вставить в адресную строку браузера и получить ответ в виде JSON
                      ; (e.g. {"response":{"count":377421,"items":[56,121,134,149,175,193,243,341,364,404]}} )
                      "https://api.vk.com/method/groups.getMembers?group_id=~a&offset=~a&count=~a&v=~a&access_token=~a"
                      groupid
                      offset
                      count
                      VK_API_VERSION
                      (_AT) ; ключ доступ пользователя. Как его получить см. комментарии в заголовке этого файла (в районе строки 13)
                      )))))
      (cond
        ((@. res.error)
            ; если вдруг ошибка, а такое бывает, подаем сигнал на консоль:
            ; (display (format "error: ~a" (@. res.error)))
            (display (format " [error with ~a/~a] " groupid (length acc-result)))
            (flush-output)
            ; и возвращаем накопленный список id. Не пропадать же добру!
            acc-result)
        ((empty? (@. res.response.items))
            ; если пустой результат, значит все уже перебрали, возвращаем накопленный список id участнкиов
            acc-result)
        (else
            ; в остальных случаях, надо продолжать майнить айдишники:
            (get-next-users
              groupid
              (+ 1000 offset) ; запрашиваем следующую 1000 id
              (append ; добавляем полученные 1000 id
                acc-result ; к итоговому списку
                (map ; преобразуем каждый элемент списка
                  str  ; к строковому виду (потому что число id - порядковый номер регистрации пользователя, не несет большого смысла в нашем случае. В нашем случае это тупо идентификатор, который мог бы быть с одинаковым успехом и буквенно-цифровым)
                  (@. res.response.items) ; добираемся до собственно списка id в формате JSON
                ))
          )))))
  ; Инициализация считывания. Получаем чистый id группы из URL группы и запрашиваем id участников, начиная с первой тысячи.
  ; Тем самым запускаем рекурсию (точнее итерацию), которая будет крутиться, пока не выгребет все айдишники:
  (let* ((groupid (extract-pure-alias groupid)))
    (get-next-users groupid 0)))

(define-catch (get-group-contacts groupid
                            #:p (p #f))
  ; (--- (format
  ;         "https://api.vk.com/method/groups.getById?group_id=~a&filter=contacts&v=~a&access_token=~a"
  ;         groupid
  ;         VK_API_VERSION
  ;         (_AT)))
  (print-with-delay p)
  (let ((res (string->jsexpr
                (get-url
                  (format
                    "https://api.vk.com/method/groups.getById?group_id=~a&fields=contacts&v=~a&access_token=~a"
                    groupid
                    VK_API_VERSION
                    (_AT)
                    )))))
    (cond
      ((vk-error? res)
          (display (format " [error with ~a] " groupid))
          (flush-output)
          #f)
      (else
        (let* ((first-group (first (@. res.response)))
              (contact-blocks ($ contacts first-group))
              (ids (if contact-blocks
                      (map (λ (contact-block) ($ user_id contact-block)) contact-blocks)
                      empty)))
          ids)))))

(define (vk-error? res)
  (or
    (@. res.error)
    (@. res.error_code)
    (@. res.error_message)))

(define (vk/intersect-groups . groups)
  (when (status-output) (display "\nscanning groups: \n"))
  (apply set-intersect (map get-group-users groups)))

(define (vk/minus-groups . groups)
  (when (status-output) (display "\nscanning groups: \n"))
  (apply minus (map get-group-users groups)))

(define (vk/difference-groups . groups)
  (when (status-output) (display "\nscanning groups: \n"))
  (apply difference (map get-group-users groups)))

; for expressions in the command line:
(define-macro (g-op op)
  `(λ groups
    (let
      ((uids-by-groups
          (map
            (λ (group)
              (cond
                ((list? group) group)
                ((string? group) (get-group-users group))
                (else group)))
            groups)))
      (apply ,op uids-by-groups))))

(define g+ (g-op append-unique))
(define g^ (g-op set-intersect))
(define g- (g-op minus))
(define g-- (g-op difference))

;;; Group wall ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-catch (get-wall-posts
          id
          #:group? (group? #t)
          #:offset (offset #f)
          #:limit (limit #f)
          #:filtered-by (filtered-by "owner")
          #:extended (extended #f)
          #:success-display (success-display #f)
          #:break-if-error (break-if-error #t)
          #:on-closed-wall (on-closed-wall #f)
          #:do-when-error (do-when-error #f))
  (let* ((reqstr (format "https://api.vk.com/method/wall.get?owner_id=~a~a&v=~a&~a~a~a~a&access_token=~a"
                    (if group? "-" "")
                    id
                    VK_API_VERSION
                    (if extended "&extended=1" "")
                    (format "&filter=~a" filtered-by)
                    (if offset (str "&offset=" offset) "")
                    (if limit (str "&count=" limit) "")
                    (_AT)))
        (res (string->jsexpr
                    (get-url reqstr)))
        (response ($ response res))
        (err ($ error res))
        (closed-wall? (and err (equal? ($ error_code err) ($ THE_PROFILE_IS_PRIVATE VK_ERROR_MESSAGES))))
        (profile-deleted? (and err (equal? ($ error_code err) ($ THE_PROFILE_IS_BLOCKED_OR_DELETED VK_ERROR_MESSAGES)))))
    (cond
      ((or closed-wall? profile-deleted?)
        ; (--- do-when-error (do-when-error err))
        (and on-closed-wall (on-closed-wall id))
        (and do-when-error (do-when-error err)))
      ((and err break-if-error) (error err))
      ((and err do-when-error) (do-when-error err) #f)
      (err #f)
      (else
        (when success-display
              (display success-display)
              (flush-output))
        response))))

(define-catch (get-attachments item)
  ; (let* ((copy_history ($ copy_history item))
  ;       (copy_history (and (not-empty? copy_history) (first copy_history)))
  ;       (attachments (or
  ;                       (and copy_history ($ attachments copy_history))
  ;                       ($ attachments item))))
    ; attachments))
    (cond
      ((repost? item) ($ copy_history.attachments item))
      (else ($ attachments item))))

(define-catch (get-attachment-element attachments type)
  (let* (
        (elements (and attachments (filter-map (λ (x) (hash-ref* x type)) attachments)))
        (element (and elements (not-empty? elements) (first elements)))
        (sizes (and
                  element
                  (cond
                    ((equal*? type 'photo) ($ sizes element))
                    (else ($ photo.sizes element)))))
        (urls (and sizes
                      (for/hash
                        ((size sizes))
                        (values ($ width size) ($ url size))))))
    (if urls urls (hash))))

(define-catch (get-img-urls item)
  (let* (
        (attachments (get-attachments item))
        ; attachment photo
        (photo_urls (get-attachment-element attachments 'photo))
        (photo_urls (hash
                      '1x ($ 75 photo_urls)
                      '2x ($ 130 photo_urls)
                      '3x (or ($ 604 photo_urls) ($ 605 photo_urls) ($ 509 photo_urls) ($ 510 photo_urls))
                      '4x ($ 807 photo_urls)
                      '5x ($ 1280 photo_urls)
                      ; if no any pictures with specified widths, ok, you win, let's take the biggest one for a fallback
                      'largest (and (not (hash-empty? photo_urls))
                        (first (sort (hash-keys photo_urls) (λ (x y) (> x y)))))))

        ; attachment link
        (link_urls (get-attachment-element attachments 'link))
        (link_urls (hash
                      '1x-link ($ 75 link_urls)
                      '2x-link ($ 130 link_urls)
                      '3x-link ($ 537 link_urls)
                      '4x-link #f
                      '5x-link ($ 1074 link_urls)))
        ; attachment doc (TODO: upgrade)
        (doc_urls (and attachments ($ doc.url attachments)))
        (doc_urls (if doc_urls (hash 'doc doc_urls) (hash)))
        )
    ; (--- "photo" ($ photo attachment))
    ; (--- "photo.photo_604" ($ photo.photo_604 attachment))
      (hash-union photo_urls link_urls doc_urls)
    ))

(define-catch (get-video-img-urls item)
  (let* (
        (attachments (get-attachments item))
        (attachment (or (and attachments (not-empty? attachments) (first attachments)) (hash)))
        )
    (hash
      '2x (and attachments ($ video.photo_130 attachment))
      '3x (and attachments ($ video.photo_320 attachment))
      '4x (and attachments ($ video.photo_800 attachment))
      '5x (and attachments ($ video.photo_1280 attachment))
      '1x_first_frame (and attachments ($ video.first_frame_130 attachment))
      '2x_first_frame (and attachments ($ video.first_frame_160 attachment))
      '3x_first_frame (and attachments ($ video.first_frame_320 attachment))
      '4x_first_frame (and attachments ($ video.first_frame_800 attachment))
      '5x_first_frame (and attachments ($ video.first_frame_1280 attachment))
    )))

(define-catch (get-img-url item #:image-size (image-size '2x))
  (hash-ref (get-img-urls item) image-size #f))

(define-catch (post-contains-img? item)
  (ormap
    true?
    (hash-values (get-img-urls item))))

(define-catch (attachment-is-a-photo? attachment)
  (equal? ($ type attachment) "photo"))

(define-catch (get-photo-id attachment)
  (and
    (attachment-is-a-photo? attachment)
    ($ photo.id attachment)))

(define-catch (move-post-images item group_id target_album_id #:delay (delay-time #f) #:count-parameter (count #f) #:break-if-error (break-if-error #t) #:do-when-error (do-when-error #f))
  (and
    (post-contains-img? item)
    (let* ((attachments (get-attachments item))
          (photo_ids (map
                        get-photo-id
                        attachments)))
      (for ((photo_id photo_ids))
        ; https://vk.com/dev/photos.delete?params[owner_id]=231485211&params[photo_id]=305326320&params[v]=5.107
        (let* (
              (_ (when count (count (+ (count) 1)) #t))
              (_ (--- photo_id (when count (count))))
              ; (_ (--- group_id target_album_id photo_id VK_API_VERSION (_AT)))
              (reqstr (format "https://api.vk.com/method/photos.move?owner_id=-~a&target_album_id=~a&photo_id=~a&v=~a&access_token=~a"
                                group_id
                                target_album_id
                                photo_id
                                VK_API_VERSION
                                (_AT)))
              (_ (when delay-time (sleep delay-time)))
              (res (string->jsexpr
                          (get-url reqstr)))
              (response ($ response res))
              (err ($ error res)))
          (cond
            ((and err break-if-error) (error err))
            ((and err do-when-error) (do-when-error err) #f)
            (err #f)
            (else
              response)))))))

; works only for groups, for users it might be implemented later
(define-catch (delete-post-images item group_id #:delay (delay-time #f) #:count-parameter (count #f) #:break-if-error (break-if-error #t) #:do-when-error (do-when-error #f))
  (and
    (post-contains-img? item)
    (let* ((attachments (get-attachments item))
          (photo_ids (map
                        get-photo-id
                        attachments)))
      (for ((photo_id photo_ids))
        ; https://vk.com/dev/photos.delete?params[owner_id]=231485211&params[photo_id]=305326320&params[v]=5.107
        (let* (
              (_ (when count (count (+ (count) 1)) #t))
              ; (_ (--- photo_id (when count (count))))
              (reqstr (format "https://api.vk.com/method/photos.delete?owner_id=-~a&photo_id=~a&v=~a&access_token=~a"
                                group_id
                                photo_id
                                VK_API_VERSION
                                (_AT)))
              (_ (when delay-time (sleep delay-time)))
              (res (string->jsexpr
                          (get-url reqstr)))
              (response ($ response res))
              (err ($ error res)))
          (cond
            ((and err break-if-error) (error err))
            ((and err do-when-error) (do-when-error err) #f)
            (err #f)
            (else
              response)))))))

; (define-catch (get-all-images group_id offset #:delay (delay-time #f) #:break-if-error (break-if-error #t) #:do-when-error (do-when-error #f))
;   (let* (
;         (reqstr (format "https://api.vk.com/method/photos.getAll?owner_id=-~a&extended=~a&offset=~a&count=~a&v=~a&access_token=~a"
;                           group_id
;                           0
;                           offset
;                           200
;                           VK_API_VERSION
;                           (_AT)))
;         (_ (when delay-time (sleep delay-time)))
;         (res (string->jsexpr
;                     (get-url reqstr)))
;         (response ($ response res))
;         (err ($ error res)))
;     (cond
;       ((and err break-if-error) (error err))
;       ((and err do-when-error) (do-when-error err) #f)
;       (err #f)
;       (else
;         response))))

(define (vk-error-msg res)
  (@. res.error.error_msg))

(define-catch (get-post-text item)
  ; (when (equal? ($ id item) 3794338)
  ;   (--- "vk.rkt/get-post-text:" (repost? item) ($ copy_history.text item)))
  (cond
    ((repost? item) ($ copy_history.text item))
    (else ($ text item))))
