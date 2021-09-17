;;; leo.el --- Interface for dict.leo.org -*- lexical-binding:t -*-
;;
;; Copyright (C) 2020 M.T. Enders <michael AT enders.io>
;;
;; Author: M.T. Enders <michael AT enders.io>
;; Created: 21 Oct 2020
;;
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, translate
;; URL: https://github.com/mtenders/emacs-leo
;; Version: 0.1
;; Prefix: leo
;; Separator: -

;;; Commentary:
;;
;; A simple interface for dict.leo.org.
;;
;; Usage:
;;
;; This provides the commands leo-translate-word and
;; leo-translate-at-point.  Both translate from the language set by the
;; custom variable leo-language to German.
;;
;; Available languages: en, es, fr, it, ch, pt, ru, pl

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'xml)

(defcustom leo-language "en"
  "Language to translate from to German.

Available languages: en, es, fr, it, ch, pt, ru, pl"
  :type 'string
  :group 'leo
  :options '("es" "fr" "it" "ch" "pt" "ru" "pl"))

(defface leo--link-face
  '((t :inherit warning))
  "Face used for links to forum posts.")

(defface leo--auxiliary-face
    '((t :inherit font-lock-comment-face))
    "Face used to fade auxiliary items.")

(defface leo--match-face
    '((t :inherit success :weight bold))
    "Face used for search terms in search results.")

(defun leo--generate-url (lang word)
  "Generate link to query for translations of WORD from LANG to German."
  (concat
   "https://dict.leo.org/dictQuery/m-vocab/"
   lang
   "de/query.xml?tolerMode=nof&lp="
   lang
   "de&lang=de&rmWords=off&rmSearch=on&search="
   word
   "&searchLoc=0&resultOrder=basic&multiwordShowSingle=on"))

(defun leo--parse-xml (url)
  "Parse xml file retrieved from URL."
  (with-temp-buffer
	(url-insert-file-contents url)
	(xml-parse-region (point-min) (point-max))))

(defun leo--extract-translations (lst &optional acc)
  "Extract translations from LST and add it to ACC.
The returned list contains strings of alternating languages"
  (while (consp lst)
    (setq acc (cons (mapconcat 'caddr (cddr (pop lst)) ", ") acc)))
  (reverse acc))

(defun leo--pair-translations (lst)
  "Take the elements from LST and pair them."
  (if (null lst)
      nil
    (cons (cons (car lst) (cadr lst)) (leo--pair-translations (cddr lst)))))

(defun leo--map-get-children (seq child)
  "Map xml-get-children over SEQ for CHILD."
  (mapcan
   (lambda (node) (xml-get-children node child))
   seq))

(defun leo--get-child (tag child)
    "Why do I fucking have to do this?"
  (car (xml-get-children tag child)))

(defun leo--extract-cases-from-side (side)
  "Extract a term's case from a given SIDE.
A side is either the source or target result for a given search."
  (let* ((repr (xml-get-children side 'repr))
         (smalls (leo--map-get-children repr 'small))
         (sups (leo--map-get-children smalls 'sup))
         (ms (leo--map-get-children sups 'm))
         (cases (leo--map-get-children ms 't)))
    (or (mapcar (lambda (x)
                  (caddr x))
                cases)
        ""))) ; handle no case markers
(defun leo--extract-lang-from-side (side)
  "Get the language that SIDE is in."
  (let ((info (cadr side)))
    (cdadr info)))

(defun leo--strip--trailing-period (string)
  "Remove trailing period from STRING."
  ;;TODO test if it actually ends with a period
  ;; for now just run it on EN tags, which all seem to
  ;; & DE case markers
  (substring string 0 -1))

(defun leo--extract-tag-from-side (side)
  "Extract a term's domain tag from a given SIDE.
A side is either the source or target result for a given search."
  (let* ((repr (car (xml-get-children side 'repr)))
	     (domain (car (xml-get-children repr 'domain)))
         (small (car (xml-get-children domain 'small)))
         (m (car (xml-get-children small 'm)))
         (tag (car (xml-get-children m 't))))
    (or (caddr tag)
        ""))) ; handle no tag

(defun leo--extract-plural-from-side (side)
  "Extract a term's plural form from a given SIDE.
A side is either the source or target result for a given search.
Returns a string ."
  (let* ((repr (car (xml-get-children side 'repr)))
	     (flecttabref (car (xml-get-children repr 'flecttabref)))
         (small (car (xml-get-children flecttabref 'small))))
         ;; (m (car (xml-get-children small 'm)))
         ;; (tag (car (xml-get-children m 't))))
    (or (cadddr small)
        ""))) ; handle no plural

(defun leo--extract-flextable-from-side (side)
  "Extract the link to a term's inflexion table from a given SIDE.
A side is either the source or target result for a given search.
Returns a string ."
  (let* ((base-url "https://dict.leo.org/pages/flecttab/flectionTable.php?kvz=")
         (ibox (car (xml-get-children side 'ibox)))
         (flexmain (car (xml-get-children ibox 'flexmain)))
         (url-suffix (cdr (assoc 'table (car (cdaddr flexmain))))))
	     ;; (flecttab (car (xml-get-children ibox 'flecttab))) ; NB: DEPRECATED!
         ;; (url-suffix (cdar (cddr (cadr flecttab)))))
    (if flexmain
        (or (concat base-url url-suffix)
            ""))))

(defun leo--extract-translations-and-tags (sides &optional words-tags-list)
  "Extract translations and tags from list SIDES.
Returns nested list of term/tag cons cells, for both languages"
  (while (consp sides)
    (setq words-tags-list
          (cons (list
                 (caddar (cddr (car (leo--map-get-children sides 'words))))
                 (leo--extract-tag-from-side (car sides))
                 (leo--extract-plural-from-side (car sides))
                 (leo--extract-flextable-from-side (car sides))
                 (leo--extract-cases-from-side (car sides)))
                words-tags-list))
    (pop sides))
  (reverse words-tags-list))

(defun leo--extract-translation-pairs (parsed-xml)
  "Extract translation pairs from PARSED-XML."
  (let* ((sectionlist (leo--map-get-children parsed-xml 'sectionlist))
	 (section (leo--map-get-children sectionlist 'section))
	 (entry (leo--map-get-children section 'entry))
	 (side (leo--map-get-children entry 'side)))
    (leo--pair-translations (leo--extract-translations-and-tags side))))

(defun leo--extract-forum-subject-link-pairs (parsed-xml)
  "Extract forum entry names and links from PARSED-XML.
Returns a nested list of forum posts titles, urls, and teasers."
  (let* ((forumref (leo--map-get-children parsed-xml 'forumRef))
         (forumref-link (leo--map-get-children forumref 'link)))
         (mapcar (lambda (x)
                   (list
                    (nth 2 (nth 2 x)) ; subject
                    (cdr (assoc 'href (nth 1 x))) ; href
                    (nth 2 (nth 3 x)))) ; teaser
                 forumref-link)))

(defun leo--print-translation (pairs)
  "Format and print translation PAIRS.
Results include domain tags and plural forms."
  (let ((src (caaar pairs))
        (src-tag (cadaar pairs))
        (src-plr (car (cddaar pairs)))
        (targ (cadar pairs))
        (targ-tag (caddar pairs))
        (targ-plr (car (cdddar pairs)))
        (flextab-url (cadr (cdddar pairs)))
        (cases (caddr (cdddar pairs))))
    (if (null pairs) nil
      (with-current-buffer (get-buffer " *leo*")
        (insert
         (concat
          src
          (if (not (eq src-tag ""))
              (propertize (concat " [" src-tag "]")
                          'face 'leo--auxiliary-face))
          (if (not (eq src-plr ""))
              (propertize (concat " pl." src-plr)
                          'face 'leo--auxiliary-face))
          "\n       "
          (propertize "-> "
                      'face 'leo--auxiliary-face)
          targ
          (if (not (eq targ-tag ""))
            (propertize (concat " [" targ-tag "]")
                        'face 'leo--auxiliary-face))
          (if (not (eq targ-plr ""))
              (propertize (concat targ-plr)
                          'button t
                          'follow-link t
                          'shr-url flextab-url
                          'keymap shr-map
                          'fontified t
                          'face 'leo--auxiliary-face
                          'mouse-face 'highlight
                          'help-echo (concat "Browse inflexion table for '" targ "'")))
          " "
          (if (not (eq cases ""))
              (propertize (concat "(" (mapconcat #'identity cases ", ") ")") 'face 'leo--auxiliary-face ))
          (propertize " | " 'face 'leo--auxiliary-face)
          (propertize "table"
                      'button t
                      'follow-link t
                      'shr-url flextab-url
                      'keymap shr-map
                      'fontified t
                      'face 'leo--link-face
                      'mouse-face 'highlight
                      'help-echo (concat "Browse inflexion table for '" targ "'"))
          "\n\n")))
    (leo--print-translation (cdr pairs)))))

(defun leo--print-forums (forum-posts)
  "Format and print translation FORUM-POSTS."
  (if (null forum-posts) nil
    (let* ((url (concat "https://dict.leo.org" (car (cdar forum-posts))))
           (post-title
            (propertize (caar forum-posts)
                        'button t
                        'follow-link t
                        'shr-url url
                        'keymap shr-map
                        'fontified t
                        'face 'leo--link-face
                        'mouse-face 'highlight
                        'help-echo (concat "Browse forum entry for '" (caar forum-posts) "'")))
           (teaser
            (propertize (nth 2 (car forum-posts))
                        'face 'leo--auxiliary-face)))
      (with-current-buffer (get-buffer " *leo*")
        (insert
         (concat
          post-title
          "\n"
          teaser
          "\n\n"
          (leo--print-forums (cdr forum-posts))))))))

(defun leo--propertize-search-term-in-results (word)
  "Add `leo--match-face' to any instances of WORD in results buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp word nil 'noerror)
      (let ((props (text-properties-at (- (point) 1))))
        (remove-text-properties (- (point) (length word)) (point)
                                '(face face))
        (add-text-properties (- (point) (length word)) (point)
                             '(face leo--match-face))))))

(defun leo--open-translation-buffer (pairs forums word)
  "Print translation PAIRS and FORUMS in temporary buffer.
The search term WORD is propertized in results."
  (with-output-to-temp-buffer " *leo*"
    (princ (concat "leo.de search results for " word ":\n\n"))
    (leo--print-translation pairs)
    (princ (concat "\n\nleo.de forum results for " word ":\n\n"))
    (leo--print-forums forums))
  (if (not (equal (buffer-name (current-buffer)) " *leo*"))
      (other-window 1))
  (let ((inhibit-read-only t))
    (leo--propertize-search-term-in-results word))
  (local-set-key (kbd "<tab>") 'shr-next-link)
  (local-set-key (kbd "<backtab>") 'shr-previous-link))

(defun leo--translate (lang word)
  "Translate WORD from LANG to German."
  (let ((xml (leo--parse-xml
              (leo--generate-url lang word))))
    (leo--open-translation-buffer
     (leo--extract-translation-pairs xml)
     (leo--extract-forum-subject-link-pairs xml)
     word)))

(defun leo--inspect-parsed-xml (word)
  "View the parsed xml returned by a query for WORD."
  (interactive "sTranslate: ")
  (let ((xml (leo--parse-xml
              (leo--generate-url leo-language word)))
        (buffer (get-buffer-create "*leo-parsed-xml*")))
    ;; (with-current-buffer buffer
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (goto-char (point-min))
    (setq inhibit-read-only t)
      ;; (emacs-lisp-mode)
    (print xml (current-buffer))
    (emacs-lisp-mode)
    (pp-buffer)))

      ;; (pp-buffer))))

;;;###autoload
(defun leo-translate-word (word)
  "Translate WORD from language set by 'leo-language' to German.
Show translations in new buffer windown."
  (interactive "sTranslate: ")
  (leo--translate leo-language word))

;;;###autoload
(defun leo-translate-at-point ()
  "Translate word under cursor from language set by 'leo-language' to German.
Show translations in new buffer windown."
  (interactive)
  (leo--translate leo-language (current-word)))


(provide 'leo)
;;; leo.el ends here
