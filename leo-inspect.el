
(defun leo--inspect-parsed-xml (word)
  "View the parsed xml returned by a query for WORD.
The result is also saved to variable leo-xml-inspect-result for playing."
  (interactive "sTranslate: ")
  (let ((xml (leo--parse-xml
              (leo--generate-url leo-language word)))
        (buffer (get-buffer-create "*leo-parsed-xml*")))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (setq leo-xml-inspect-result xml)
      (setq leo-xml-inspect-section-list
            (car (leo--get-result-section-list
                  (car leo-xml-inspect-result))))
      (setq leo-xml-inspect-sections-as-list
            (leo--get-result-sections-as-list
             (leo--get-result-section-list
              (car leo-xml-inspect-result))))
      ;; get sections by POS:
      (mapcar (lambda (x)
                (let ((pos (leo--get-section-part-of-speech x)))
                  (cond
                   ((equal pos "Substantive")
                  (setq leo-xml-inspect-single-section-noun x))
                 ((equal pos "Verben")
                  (setq leo-xml-inspect-single-section-verb x))
                 ((equal pos "Adjektive/Adverbien")
                  (setq leo-xml-inspect-single-section-adj-adv x))
                 ((equal pos "Präpositionen/Pronomen")
                  (setq leo-xml-inspect-single-section-praep x))
                 ((equal pos "Beispeiele")
                  (setq leo-xml-inspect-single-section-eg x))
                 ((equal pos "Wendungen/Ausdrücke")
                  (setq leo-xml-inspect-single-section-wendung x))
                 (t
                  (setq leo-xml-inspect-single-section-other x)))))
              leo-xml-inspect-sections-as-list)
      (setq leo-xml-inspect-single-section
            (car (leo--get-result-sections-as-list
                  (leo--get-result-section-list
                   (car leo-xml-inspect-result)))))
      (setq leo-xml-inspect-entries-list
            (leo--get-entries-from-section
             (car (leo--get-result-sections-as-list
                   (leo--get-result-section-list
                    (car leo-xml-inspect-result))))))
      (setq leo-xml-inspect-single-entry
            (car (leo--get-entries-from-section
                  (car (leo--get-result-sections-as-list
                        (leo--get-result-section-list
                         (leo--xml-node-itself xml)))))))
      (setq leo-xml-inspect-single-side-1
            (car (leo--get-sides-from-entry
                  leo-xml-inspect-single-entry)))
      (setq leo-xml-inspect-single-side-2
            (cadr (leo--get-sides-from-entry
                  leo-xml-inspect-single-entry)))
      (print xml (current-buffer))
      (emacs-lisp-mode)
      (pp-buffer)
      )))
