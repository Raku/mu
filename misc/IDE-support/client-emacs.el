
; M-x eval-buffer
; M-x foo

(defun foo ()
  (interactive)
  (let ((src (current-buffer)))
    (with-temp-buffer
      (let ((tmp (current-buffer)))
        (set-buffer src)
        (call-process-region (point-min) (point-max) "pugs" nil tmp nil "server.pl")
        (eval-buffer tmp nil)))))
