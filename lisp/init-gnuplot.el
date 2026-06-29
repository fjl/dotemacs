(require 'rect)
(require 'gnuplot)

;;;###autoload
(defun gnuplot-rectangle (&optional title style)
  "Plot numbers in the current rectangle region, or the last killed
rectangle."
  (interactive)
  (let* ((rect (if (use-region-p)
                   (extract-rectangle (region-beginning) (region-end))
                 killed-rectangle))
         (name (make-temp-file "plot"))
         cols header n xlabel ylabel)
    (unless rect
      (user-error "No rectangle: select one (C-x SPC, rectangle-mark-mode) or kill one (C-x r r)"))
    (setq cols (split-string (car rect)))
    (when (and (car cols) (string-match-p "^[a-zA-Z]" (car cols)))
      (setq header cols
            rect   (cdr rect)))            ; first line is labels: strip it
    (setq n (length cols))                 ; column count
    (pcase n
      (1 (setq ylabel (nth 0 header)))
      (2 (setq xlabel (nth 0 header)
               ylabel (nth 1 header)))
      (_ (setq xlabel (nth 0 header))))
    ;; data file
    (with-temp-file name
      (insert (mapconcat #'identity rect "\n") "\n"))
    ;; gnuplot script
    (setq style (or style "line"))
    (with-temp-buffer
      (insert "set title  '" (or title  "Title")  "'\n")
      (insert "set xlabel '" (or xlabel "x-axis") "'\n")
      (insert "set ylabel '" (or ylabel "y-axis") "'\n")
      (let (specs)
        (if (= n 1)
            (push (concat "'" name "' using 1 with " style " notitle") specs)
          (dotimes (i (1- n))
            (let ((src  (if (= i 0) (concat "'" name "'") "''"))
                  (ycol (number-to-string (+ i 2))))
              (push (concat src " using 1:" ycol " with " style
                            (if (> n 2)
                                (concat " title '" (or (nth (1+ i) header) "") "'")
                              " notitle"))
                    specs))))
        (insert "plot " (mapconcat #'identity (nreverse specs) ", ") "\n"))
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot))))

(keymap-set gnuplot-mode-map "C-c M-i" 'gnuplot-inline-display-mode)

(provide 'init-gnuplot)
