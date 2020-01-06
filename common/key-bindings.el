;;
;; Copyright 2017 - 2020 by MaiHD
;;

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun close-emacs-or-frame ()
  (interactive)
  (if (>= 1 (length (frame-list)))
      (save-buffers-kill-emacs)
    (delete-frame (selected-frame))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta down)] 'move-line-down)
(global-set-key [(meta f4)] 'close-emacs-or-frame)
