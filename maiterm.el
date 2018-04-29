;;;
;;; MaiMacs/MaiTerm 2018, by MaiHD
;;;

(require 'comint)
(require 'pcomplete)

(defgroup maiterm nil
  ""
  :group 'processes
  :group 'unix)

(defun maiterm-compile (&optional arg)
  ""
  (interactive)
  (let ((b (get-buffer "*PowerShell*"))
	(p (get-process "shell")))
    (if (and b p)
	(with-current-buffer b
	  (apply comint-input-sender (list p compile-command))))))

(defun maiterm-clear-region ()
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))

(defun maiterm (&optional buffer)
  (interactive
   (list
    (and current-prefix-arg
	 (prog1
	     (read-buffer "MaiTerm buffer: "
			  (if (and (eq major-mode 'shell-mode)
				   (null (get-buffer-process (current-buffer))))
			      (buffer-name)
			    (generate-new-buffer-name "*maiterm*")))
	   (if (file-remote-p default-directory)
	       (setq default-directory
		     (expand-file-name
		      (read-directory-name
		       "Default directory: "
		       default-directory
		       default-directory
		       t
		       nil))))))))
  
  (setq buffer (if (or buffer (not (derived-mode-p 'maiterm-mode))
		       (comint-check-proc (current-buffer)))
		   (get-buffer-create (or buffer "*maiterm*"))
		 (current-buffer)))

  buffer)

(provide 'maiterm)
