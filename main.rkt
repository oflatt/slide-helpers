#lang slideshow/widescreen

(require pict pict/color pict/conditional slideshow/play pict/flash)
(require slideshow/slides-to-picts)

(require racket/draw)
(require slideshow/repl)

(provide (all-defined-out))

;; TODO make a magic move function that takes two slides and makes a new one with the moved item

(define BORDERWIDTH 3)
(define CORNER-RADIUS -0.15)

(define current-iter (make-parameter 0))
(define current-time (make-parameter 0))

(current-font-size 30)
(current-titlet
  (lambda (s)
  (colorize (text s (current-main-font) 50)
            (current-title-color))))


(define tl-find lt-find)
(define tr-find rt-find)
(define bl-find lb-find)
(define br-find rb-find)
(define bc-find cb-find)
(define tc-find ct-find)


;; COLORS
(define blue (make-object color% 38 132 235))
(define light-blue (light blue))
(define green (dark "green"))
(define red "red")
(define black "black")


(define (tt str)
  (text str (current-main-font) 50))

(define (ht str)
  (parameterize ([current-font-size 40])
   (t str)))

(define (st str)
  (parameterize ([current-font-size 30])
    (t str)))


;; impose pictb on to picta
(define (superimpose x y picta pictb)
  (lt-superimpose
    (vl-append (blank 0 (if (< y 0) (- y) 0))
      (ht-append (blank (if (< x 0) (- x) 0) 0) picta))
    (vl-append (blank 0 (if (> y 0) y 0))
      (ht-append (blank (if (> x 0) x 0) 0) pictb))))

;; test this one before this
(define (superimpose-better x y new base)
  (define xpos
    (if (equal? x 'center)
        (/ (- (pict-width base) (pict-width new)) 2)
        x))
  (define ypos
    (if (equal? y 'center)
        (/ (- (pict-height base) (pict-height new)) 2)
        y))
  (lt-superimpose
   base
   (inset new xpos ypos 0 0)))

(define (rect-with-border width height #:color [color "white"] #:border-color [border-color "black"] #:border-width [border-width 1])
  (superimpose 0 0
    (filled-rectangle width border-width  #:color border-color #:border-width 0)
    (superimpose 0 0
      (filled-rectangle border-width height #:color border-color #:border-width 0)
      (superimpose (- width border-width) 0
        (filled-rectangle border-width height #:color border-color #:border-width 0)
        (superimpose 0 (- height border-width)
          (filled-rectangle width border-width #:color border-color #:border-width 0)
          (filled-rectangle width height
      #:color color
      #:border-width 0))))))

;; like the append methods, but stacks the elements on top of each other
(define (stack x y . elements)
  (cond 
    [(empty? elements)
      (blank 0 0)]
    [(equal? (length elements) 1)
      (first elements)]
    [else
    (superimpose x y
      (first elements)
      (apply (curry stack x y) (rest elements)))]))


(define bitmap-cache
  (make-hash))

(define (cached-bitmap name)
  (hash-ref! bitmap-cache name
             (lambda ()
               (bitmap name))))


(define (indent . content)
  (hc-append (blank (current-gap-size) 0) (apply (curry vl-append (current-gap-size)) content)))

(define (indent2 . content)
  (indent (apply indent content)))

(define (subitem-no-bullet . content)
  (apply subitem #:bullet (blank 0 0) content))

(define (subsubitem . contents)
  (ht-append (blank (* 2 (current-gap-size)) 0) (apply subitem-no-bullet contents)))

(define (append-gap-size . elements)
  (apply (curry vl-append (current-gap-size)) elements))

(define (happend-gap-size . elements)
  (apply (curry ht-append (current-gap-size)) elements))

(define (make-dynamic-slide func #:iters [iters 1] #:title [title #f] #:layout [layout 'top] #:start [start 0])
  (for ([iter (in-range start iters)])
       (play-n #:title (cond
                         [(procedure? title) (title iter)]
                         [else title])  #:skip-first? #t #:layout layout
               (lambda (t) (func iter t)))))

(define-syntax-rule (dynamic-slide title (vars ...) body ...)
  (begin
    (match-define (list vars ...) (range 1 (+ (length `(vars ...)) 1)))
    (make-dynamic-slide
      #:iters (+ (length `(vars ...)) 1)
      #:title title
      (lambda (iter time)
        (parameterize ([current-iter iter] [current-time time])
        body ...)))))

(define (make-node text [highlighted #f])
  (define element (t text))
  (cc-superimpose (filled-rounded-rectangle
                   (max 50 (+ 10 (pict-width element)))
                   (+ 10 (pict-height element))
                   CORNER-RADIUS
                   #:color (light "gray")
                   #:border-color (if highlighted blue "black")
                   #:border-width (if highlighted (* BORDERWIDTH 2) BORDERWIDTH))
                  element))

(define (parent-child combined node1 node2)
  (pin-arrow-line 15 combined
                  node1 cb-find
                  node2 ct-find
                  #:line-width BORDERWIDTH))

(define (autoarrow node1 node2 contents #:alpha [alpha 1] #:arrow-size [arrow-size 25] #:style [style #f] #:line-width [line-width (* 2 BORDERWIDTH)] #:finder1 [finder1 #f] #:finder2 [finder2 #f] #:start-angle [start-angle #f] #:end-angle [end-angle #f] #:label [label (blank)] #:x-adjust-label [x-adjust-label 0] #:y-adjust-label [y-adjust-label -10] #:color [color #f])
  (define-values (a b) (lt-find contents node1))
  (define-values (c d) (lt-find contents node2))
  (match-define (list afind1 afind2)
    (cond
      [(> c (+ a (pict-width node1))) ;; right
       (list rc-find lc-find)]
      [(< (+ c (pict-width node2)) a) ;; left
       (list lc-find rc-find)]
      [(and (> d (+ b (pict-height node1)))) ;; underneath
       (list cb-find ct-find)]
      [(and (< (+ d (pict-height node2)) b)) ;; above
       (list ct-find cb-find)]
      [else
       (list lc-find rc-find)])) ;; default left I guess
  (define find1 (if finder1 finder1 afind1))
  (define find2 (if finder2 finder2 afind2))
  
  (pin-arrow-line arrow-size contents
   node1 find1
   node2 find2
   #:alpha alpha
   #:style style
   #:start-pull 0.075
   #:end-pull 0.075
   #:line-width line-width
   #:start-angle start-angle
   #:end-angle end-angle
   #:label label
   #:x-adjust-label x-adjust-label
   #:y-adjust-label y-adjust-label
   #:color color
   ))


(define (autoarrow-at iter current-iter time node1 node2 contents #:arrow-size [arrow-size 25] #:style [style #f] #:line-width [line-width (* 2 BORDERWIDTH)] #:finder1 [finder1 #f] #:finder2 [finder2 #f] #:start-angle [start-angle #f] #:end-angle [end-angle #f] #:label [label (blank)] #:x-adjust-label [x-adjust-label 0] #:y-adjust-label [y-adjust-label -10] #:color [color #f])
  (if (equal? iter current-iter)
      (autoarrow node1 node2 contents #:alpha time #:arrow-size arrow-size #:style style #:line-width line-width  #:finder1 finder1 #:finder2 finder2 #:start-angle start-angle #:end-angle end-angle #:label label #:x-adjust-label x-adjust-label #:y-adjust-label y-adjust-label #:color color)
           (if (> current-iter iter)
               (autoarrow node1 node2 contents #:arrow-size arrow-size #:style style #:line-width line-width #:finder1 finder1 #:finder2 finder2 #:start-angle start-angle #:end-angle end-angle #:label label #:x-adjust-label x-adjust-label #:y-adjust-label y-adjust-label
                          #:color color)
                    contents)))

(define (equality-horiz combined node1 node2 [label ""] #:x-adjust-label [x-adjust-label 0] #:y-adjust-label [y-adjust-label 0])
  (pin-line combined
            node1 rc-find
            node2 lc-find
            #:line-width BORDERWIDTH
            #:label (st label)
            #:x-adjust-label x-adjust-label
            #:y-adjust-label y-adjust-label
            #:style 'long-dash))

(define (make-bubble #:width [width #f] #:height [height #f] #:padding [padding 30] #:color [color blue] #:inner-color [inner-color (light (light color))] . children)
  (define child (apply append-gap-size children))
  (define w (if width width (+ (* padding 2) (pict-width child))))
  (define h (if height height (+ (* padding 2) (pict-height child))))
  (define rect (filled-rounded-rectangle
    w
    h
    CORNER-RADIUS
    #:color inner-color
    #:border-color color
    #:border-width BORDERWIDTH))
  (define x (/ (- w (pict-width child)) 2))
  (define y (/ (- h (pict-height child)) 2))
  (pin-over
    rect
    x y
    child))

(define (make-flash #:width [width #f] #:height [height #f]  #:padding [padding 30] #:color [color blue] #:inner-color [inner-color (light (light color))]  . children)
  (define child (apply append-gap-size children))
  (define w (if width width (+ (* padding 2) (pict-width child))))
  (define h (if height height (+ (* padding 2) (pict-height child))))
  (define filled (linewidth BORDERWIDTH (colorize (filled-flash w h) inner-color)))
  (define outline (linewidth BORDERWIDTH (colorize (outline-flash w h) color)))
  (define rect (lt-superimpose filled outline))
  (define x (/ (- w (pict-width child)) 2))
  (define y (/ (- h (pict-height child)) 2))
  (pin-over
   rect
   x y
   child))

(define (fade-iter iter pict)
   (pict-if (> (current-iter) iter)
            pict
            (pict-if (equal? iter (current-iter))
                     (cellophane pict (current-time))
                     (ghost pict))))

(define (move-anim iter x0 y0 x1 y1 pict)
  (if (> (current-iter) iter)
      (superimpose x1 y1 (blank 0 0))
      (if (equal? iter (current-iter))
          (superimpose (+ x0 (* t (- x1 x0))) (+ y0 (* t (- y1 y0))) (blank 0 0) pict)
              pict
              )))

(define (highlight-iter iter current-iter pict)
  (pict-if (>= (current-iter) iter)
           (blue pict)
           pict))

(define (combine-at iter pict1 pict2 #:combine [combine cc-superimpose])
  (pict-if (equal? iter (current-iter))
           (cc-superimpose pict1 (cellophane pict2 (current-time)))
           (pict-if #:combine combine
                    (> (current-iter) iter)
                    pict2
                    pict1)))

(define (show-at-iter iter pict #:alt [alt #f])
  (define temp
    (if alt
        alt
        (blank (pict-width pict) (pict-height pict))))
  (swap-at iter
    temp
    (swap-at (+ iter 1) pict temp)))

(define (swap-at iter pict1 pict2 #:combine [combine lt-superimpose])
  (pict-if (equal? iter (current-iter))
          (combine (cellophane pict2 (current-time))
                   (cellophane pict1 (- 1 (current-time))))
          (pict-if #:combine combine
           (> (current-iter) iter)
           pict2
           pict1)))

(define (swap-instant iter pict1 pict2 #:combine [combine lt-superimpose])
  (pict-if (equal? iter (current-iter))
          (combine (cellophane pict2
                      (if (> (current-time) 0.5) 1.0 0.0))
                   (cellophane pict1 (if (<= (current-time) 0.5) 1.0 0.0)))
          (pict-if #:combine combine
           (> (current-iter) iter)
           pict2
           pict1)))

(define (swap-temporary iter pict1 pict2)
  (swap-at (+ iter 1)
          (swap-at iter pict1 pict2)
          pict1))

(define (cycle-through start-iter picts #:combine [combine lbl-superimpose])
 (let loop ([iter (+ (length picts) -1 start-iter)] [todo (reverse picts)])
    (cond
      [(empty? todo)
       (blank 0 0)]
      [else
        (pict-if #:combine combine
                (equal? (current-iter) iter)
                 (combine
                   (cellophane (first todo) (current-time))
                   (if (> (length todo) 1)
                       (cellophane (second todo) (- 1 (current-time)))
                       (blank 0 0)))
                 (pict-if #:combine combine
                          (> (current-iter) iter)
                          (first todo)
                          (loop (- iter 1) (rest todo))
                          ))])))

(define (scale-to-height height pict)
  (define factor (/ height (pict-height pict)))
  (scale pict factor))

(define (scale-to-width width pict)
  (define factor (/ width (pict-width pict)))
  (scale pict factor))

(define (my-scale-to-fit width height pict)
  (define factor
    (min (/ width (pict-width pict))
         (/ height (pict-height pict))))
  (scale pict factor))

(define (clip-to-fit picture width height)
  (define factor (/ height (pict-height picture)))
  (define left-factor (max 0 (/ (- (* factor (pict-width picture)) width) 2)))
  (inset/clip (scale picture factor) (- left-factor) 0 (- left-factor) 0))

(define (my-make-outline section-headings section-funcs progress #:iters [iters 1] #:start [start 0])
  (define box
    (make-bubble #:color "dark gray" #:width 40 #:height 40 (blank 0 0)))

  (make-dynamic-slide #:iters iters #:layout 'center #:start start
    (lambda (iter time)
      (parameterize ([current-iter iter] [current-time time])
      (apply (curry vl-append (* 1 (current-gap-size)))
             (for/list ([heading section-headings] [func section-funcs] [i (in-range (length section-headings))])
               (vl-append (current-gap-size)
                          (hc-append
                            (* 2 (current-gap-size))
                            (pict-if (< i progress)
                                    (cc-superimpose
                                      box
                                      (fade-iter
                                        (if (equal? (+ i 1) progress)
                                            start
                                            -1)
                                        (scale (bitmap "check.png") 0.2)))
                                    box)
                            (tt heading))
                          (if func (func) (blank))
                          )))))))
        


(define robot-happy
  (bitmap "assets/robot/smile.png"))

(define robot-sad
  (bitmap "assets/robot/frown.png"))

(define robot-idea
  (scale (bitmap "assets/robot/ideacropped.png") 0.5))

(define small-robot-happy
  (scale robot-happy 0.15))

(define small-robot-sad
  (scale robot-sad 0.15))

(define small-robot-idea
  (scale robot-idea 0.15))

(define (cross-out pict)
  (pin-line
    (pin-line pict pict lt-find pict rb-find #:color "red" #:line-width (* 2 BORDERWIDTH))
    pict lb-find pict rt-find #:color "red" #:line-width (* 2 BORDERWIDTH)))

(define (superimpose-on dx dy location-pict pict base #:finder [finder tl-find])
  (define-values (x y) (finder base location-pict))
  (superimpose (+ x dx) (+ y dy) base pict ))

(define (superimpose-center dx dy pict base)
  (define x (- (/ (pict-width base) 2.0)
               (/ (pict-width pict) 2.0)))
  (define y (- (/ (pict-height base) 2.0)
               (/ (pict-height pict) 2.0)))
  (superimpose  (+ x dx) (+ y dy) base pict))

(define (my-pin-over dx dy pict base)
  (pin-over base dx dy pict))
(define (pin-on dx dy location-pict pict base #:finder [finder tl-find] #:align-x [align-x 'left] #:align-y [align-y 'top])
  (define-values (x y) (finder base location-pict))
  (define x-adjusted
    (match align-x
      ['left x]
      ['right (- x (pict-width pict))]
      ['center (- x (/ (pict-width pict) 2))]))
  (define y-adjusted
    (match align-y
      ['top y]
      ['bottom (- y (pict-height pict))]
      ['center (- y (/ (pict-height pict) 2))]))
  (pin-over base (+ x-adjusted dx) (+ y-adjusted dy) pict))

(define (image->bitmap image)
  (let* ([width (pict-width image)]
         [height (pict-height image)]
         [bm (make-bitmap width height)]
         [dc (send bm make-dc)])
    (send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    bm))

(define (images->pdf images output-file [mag 1.0] [padding-x 0] [padding-y 0])
  (define new-mag-x (* mag (- 1 padding-x)))
  (define new-mag-y (* mag (- 1 padding-y)))
  (define dc
    (new pdf-dc%
         [ interactive #f ]
         [ use-paper-bbox #f ]
         [ as-eps #f]
         [ width (* 0.8 (pict-width (first images)))]   ; Default scale is 0.8
         [ height (* 0.8 (pict-height (first images)))]
         [ output output-file ]))

  (send* dc
    (scale new-mag-x new-mag-y)
    (start-doc "useless string"))

  (for ([image images])
    (send* dc (start-page))
    (send dc draw-bitmap (pict->bitmap image) padding-x padding-y)
    (send* dc (end-page)))

  (send* dc
    (end-doc)))


(define (slides->pdf input-file output-file)
  (define real-w 1920)
  (define real-h 1080)

  (define port (open-output-file output-file #:exists 'replace))
  (define picts
    (get-slides-as-picts
     input-file real-w
     real-h
     #t))

  (images->pdf picts port 1.0)
  )


(define (pretty-display-string expr)
  (define output (open-output-string))
  (pretty-display expr output)
  (get-output-string output))



(define (remove-whitespace str)
  (regexp-replace* #px"\\s" str ""))

(define (make-repls-for-code code #:repl-fun [repl-fun make-repl] #:callback [callback (lambda (repl namespace callback-box) repl)])
  (define callback-box (box (lambda () void)))
  (define eval-callback (lambda () ((unbox callback-box))))
  (for/list ([iter (in-range (* 2 (length code)))])
    (define first-n 
      (take code (floor (/ (+ iter 1) 2))))
    (define last-line
      (cond
        [(even? iter)
         (list (list-ref code (floor (/ iter 2))))]
        [else
         '()]))
    (define-values (repl namespace)
      (repl-fun last-line #:pre-content first-n #:eval-callback eval-callback))
    (callback repl namespace callback-box)))

(define (read-sexps port)
  (cond
    [(eof-object? (peek-char port))
     '()]
    [else
      (cons (read port) (read-sexps port))]))


(define (make-egglog-repl content [repl-w (* client-w 1)] [repl-h (* client-h 2/3)] #:pre-content [pre-content '()] #:eval-callback eval-callback)
  (define namespace (make-base-empty-namespace))
  (parameterize ([current-namespace namespace])
    (namespace-require 'egglog))
  (values (make-repl content repl-w repl-h #:namespace namespace #:pre-content pre-content #:eval-callback eval-callback)
          namespace))


(define (process-line content-string)
  (define split-by-lines
    (regexp-split #px"\n" content-string))
  (define without-blanks
    (for/list ([line split-by-lines]
               #:when (not (equal? (remove-whitespace line) "")))
      line))
  (string-join
    (for/list ([line without-blanks] [iter (in-naturals)])
      (if (> iter 0)
          (string-append "  " line)
          line))
          "\n"))

(define (process-code code)
  (for/list ([line code])
    (process-line line)))
    

(define (make-repl content [repl-w (* client-w 1)] [repl-h (* client-h 2/3)] #:namespace [namespace (make-base-namespace)] #:pre-content [pre-content '()]
                     #:eval-callback eval-callback)
  (apply repl-area #:width repl-w #:height repl-h #:font-size 30
             #:persist? #t
             #:wrap-text? #t
             #:make-namespace (lambda () namespace)
             #:pre-content (process-code pre-content)
             #:eval-callback eval-callback
             (process-code content)))

(define (get-port-pos port)
  (define-values
    (_line _whatever next)
    (port-next-location port))
  (- next 1))

(define (break-up-code source-string)
  (define port (open-input-string source-string))
  (port-count-lines! port)
  (let loop ([pos 0])
    (define first-sexp
      (read port))
    (cond
      [(eof-object? first-sexp)
      '()]
      [else
       (define next (get-port-pos port))
       (define syntax-string
         (substring source-string pos next))
       (flush-output (current-output-port))
       (cons syntax-string
             (loop next))])))

     
    
(define (run-command cmd)
  (define-values (process output input error)
    (apply subprocess #f #f #f cmd))
  (close-output-port input)
  (define all-output 
    (port->string output))
  (define all-error
    (port->string error))
  (println all-error)
  (subprocess-wait process))

