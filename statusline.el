(defvar statusline-color1)
(defvar statusline-color2)
(defvar statusline-text-color)

(setq statusline-color1 "#44464c")
(setq statusline-color2 "#636875")
(setq statusline-text-color "#a0b8f7")

(set-face-attribute 'mode-line nil
                    :background "#2b2d33"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :background "#33353b"
                    :box nil)

(scroll-bar-mode -1)

(defun arrow-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"15 24 2 1\",
\". c %s\",
\"  c %s\",
\".              \",
\"..             \",
\"...            \",
\"....           \",
\".....          \",
\"......         \",
\".......        \",
\"........       \",
\".........      \",
\"..........     \",
\"...........    \",
\"............   \",
\"............   \",
\"...........    \",
\"..........     \",
\".........      \",
\"........       \",
\".......        \",
\"......         \",
\".....          \",
\"....           \",
\"...            \",
\"..             \",
\".              \",
};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-right-xpm
  (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"15 24 2 1\",
\". c %s\",
\"  c %s\",
\"              .\",
\"             ..\",
\"            ...\",
\"           ....\",
\"          .....\",
\"         ......\",
\"        .......\",
\"       ........\",
\"      .........\",
\"     ..........\",
\"    ...........\",
\"   ............\",
\"   ............\",
\"    ...........\",
\"     ..........\",
\"      .........\",
\"       ........\",
\"        .......\",
\"         ......\",
\"          .....\",
\"           ....\",
\"            ...\",
\"             ..\",
\"              .\",
};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))


(defun make-xpm
  (name color1 color2 data)
  "Return an XPM image for lol data"
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (if color1 color1 "None")
            (if color2 color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar #'(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar #'(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char ".")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))
   'xpm t :ascent 'center))

(defun half-xpm
  (color1 color2)
  (make-xpm "half" color1 color2
            (make-list 18
                       (append (make-list 6 0)
                               (make-list 6 1)))))

(defun percent-xpm
  (pmax pmin we ws width color1 color2)
  (let* ((fs   (if (eq pmin ws)
                   0
                 (round (* 17 (/ (float ws) (float pmax))))))
         (fe   (if (eq pmax we)
                   17
                 (round (* 17 (/ (float we) (float pmax))))))
         (o    nil)
         (i    0))
    (while (< i 18)
      (setq o (cons
               (if (and (<= fs i)
                        (<= i fe))
                   (append (list 0) (make-list width 1) (list 0))
                 (append (list 0) (make-list width 0) (list 0)))
               o))
      (setq i (+ i 1)))
    (make-xpm "percent" color1 color2 (reverse o))))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (gensym))
	(val-sym (gensym))
	(args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
	 (if ,val-sym
	     ,val-sym
	   (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(memoize 'arrow-left-xpm)
(memoize 'arrow-right-xpm)
(memoize 'half-xpm)
(memoize 'percent-xpm)

(defvar statusline-minor-modes nil)
(defvar statusline-arrow-shape 'arrow)
(defun statusline-make-face
  (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "statusline-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)2
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
			      :foreground statusline-text-color
			      :background bg
			      :box nil))
        cface)
    nil))

(defun statusline-make-left
  (string color1 &optional color2 localmap)
  (let ((plface (statusline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string
			 'face plface
			 'mouse-face plface
			 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
		     (arrow-left-xpm color1 color2)))
       "")))

(defun statusline-make-right
  (string color2 &optional color1 localmap)
  (let ((plface (statusline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize " " 'display
		     (arrow-right-xpm color1 color2)))
     (if arrow
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string
			 'face plface
			 'mouse-face plface
			 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

(defun statusline-make-fill
  (color)
  ;; justify right by filling with spaces to right fringe,
  ;; 20 should be calculated
  (let ((plface (statusline-make-face color)))
    (if (eq 'right (get-scroll-bar-mode))
        (propertize " " 'display '((space :align-to (- right-fringe 21)))
                    'face plface)
      (propertize " " 'display '((space :align-to (- right-fringe 24)))
                  'face plface))))

(defun statusline-make-text
  (string color &optional fg localmap)
  (let ((plface (statusline-make-face color)))
    (if string
        (if localmap
            (propertize string
			'face plface
			'mouse-face plface
			'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun statusline-make (side string color1 &optional color2 localmap)
  (cond
   ((and (eq side 'right) color2)
    (statusline-make-right  string color1 color2 localmap))
   ((and (eq side 'left) color2)
    (statusline-make-left   string color1 color2 localmap))
   ((eq side 'left)
    (statusline-make-left   string color1 color1 localmap))
   ((eq side 'right)
    (statusline-make-right  string color1 color1 localmap))
   (t
    (statusline-make-text   string color1 localmap))))

(defmacro defstatusline (name string)
  "Macro to create a statusline chunk."
  `(defun ,(intern (concat "statusline-" (symbol-name name)))
     (side color1 &optional color2)
     (statusline-make side
		      (let ((result ,string))
			(cond ((listp result)
			       (format-mode-line result)) 
			      ((not (or (stringp result)
					(null result)))
			       (progn
				 " ERR"))
			      (t
			       result)))
                     color1 color2)))



(defun statusline-mouse (click-group click-type string)
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                    nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defstatusline arrow       "")

(defstatusline buffer-id
  (propertize
   (car (propertized-buffer-identification "%12b"))
   'face (statusline-make-face color1)))

(defvar statusline-buffer-size-suffix t)

(defstatusline buffer-size (propertize
                            (if statusline-buffer-size-suffix
                                "%I"
                              "%i")
                            'local-map
			    (make-mode-line-mouse-map
			     'mouse-1
			     (lambda ()
			       (interactive)
			       (setq statusline-buffer-size-suffix
				     (not statusline-buffer-size-suffix))
			       (redraw-modeline)))))

(defstatusline rmw         "%*")
(defstatusline major-mode
  (propertize mode-name
	      'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
	      'local-map (let ((map (make-sparse-keymap)))
			   (define-key map
			     [mode-line down-mouse-1]
			     `(menu-item
			       ,(purecopy "Menu Bar") ignore
			       :filter (lambda (_)
					 (mouse-menu-major-mode-map))))
			   (define-key map [mode-line mouse-2]
			     'describe-mode)
			   (define-key map [mode-line down-mouse-3]
			     mode-line-mode-menu)
			   map)))

(defstatusline process mode-line-process)

(defstatusline minor-modes
  (let ((mms (split-string (format-mode-line minor-mode-alist))))
    (apply 'concat
	   (mapcar
	    #'(lambda (mm)
		(propertize (if (string= (car mms) mm)
				mm
				     (concat " " mm))
			    'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
			    'local-map (let ((map (make-sparse-keymap)))
					 (define-key map
					   [mode-line down-mouse-1]
					   (statusline-mouse 'minor
							     'menu
							     mm))
					 
					 (define-key map
					   [mode-line mouse-2]
					   (statusline-mouse 'minor
							     'help
							     mm))
					 
					 (define-key map
					   [mode-line down-mouse-3]
					   (statusline-mouse 'minor
							     'menu
							     mm))
					 (define-key map
					   [header-line down-mouse-3]
					   (statusline-mouse 'minor
							     'menu
							     mm))
					 map)))
	    mms))))

(defstatusline row         "%4l")
(defstatusline column      "%3c")
(defstatusline percent     "%6p")
(defstatusline narrow
  (let (real-point-min real-point-max)
    (save-excursion
      (save-restriction
	(widen)
	(setq real-point-min (point-min) real-point-max (point-max))))
    (when (or (/= real-point-min (point-min))
	      (/= real-point-max (point-max)))
      (propertize "Narrow"
		  'help-echo "mouse-1: Remove narrowing from the current buffer"
		  'local-map (make-mode-line-mouse-map
			      'mouse-1 'mode-line-widen)))))

(defstatusline status      "%s")
(defstatusline global      global-mode-string)
(defstatusline emacsclient mode-line-client)

(defstatusline vc
  (when (and (buffer-file-name (current-buffer))
	     vc-mode)
    (symbol-name (vc-mode-line (buffer-file-name (current-buffer) )))))

(defstatusline percent-xpm
  (propertize "  "
	      'display
	      (let (pmax
		    pmin
		    (ws (window-start))
                                            (we (window-end)))
		(save-restriction
		  (widen)
		  (setq pmax (point-max))
		  (setq pmin (point-min)))
		(percent-xpm pmax pmin we ws 15 color1 color2))))

(setq-default mode-line-format
              (list "%e"
                    '(:eval (concat
                             (statusline-rmw            'left   nil)
                             (statusline-buffer-size    'left   nil)
                             (statusline-buffer-id      'left   nil
							statusline-color1)
			     
                             (statusline-major-mode     'left
							statusline-color1)
			     
                             (statusline-process        'text
							statusline-color1)
			     
                             (statusline-minor-modes    'left
							statusline-color1)
			     
                             (statusline-narrow         'left
							statusline-color1
							statusline-color2)
			     
                             (statusline-global         'center
							statusline-color2)
			     
                             (statusline-vc             'center
							statusline-color2)
			     
                             (statusline-make-fill      statusline-color2)
			     
                             (statusline-row            'right
							statusline-color1
							statusline-color2)
			     
                             (statusline-make-text      ":"
							statusline-color1)
                             (statusline-column         'right
							statusline-color1)
                             (statusline-percent        'right  nil
							statusline-color1)
                             (statusline-percent-xpm    'text   nil
							statusline-color1)
                             (statusline-make-text      "  "    nil)))))




(provide 'statusline)

;;; statusline.el ends here
