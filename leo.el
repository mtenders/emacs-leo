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
The returned list conains strings of alternating languages"
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

(defun leo--extract-translation-pairs (parsed-xml)
  "Extract translation pairs from PARSED-XML."
  (let* ((sectionlist (leo--map-get-children parsed-xml 'sectionlist))
	 (section (leo--map-get-children sectionlist 'section))
	 (entry (leo--map-get-children section 'entry))
	 (side (leo--map-get-children entry 'side))
	 (words (leo--map-get-children side 'words)))
    (leo--pair-translations (leo--extract-translations  words))))

(defun leo--extract-forum-subject-link-pairs (parsed-xml)
  "Extract forum entry names and links from PARSED-XML.
Returns a nested list of subject, url pairs."
  (let* ((forumref (leo--map-get-children parsed-xml 'forumRef))
         (forum (leo--map-get-children parsed-xml 'forum))
         (forum-list (leo--map-get-children forum 'link)))
          (mapcar
           (lambda (x)
             (cons (nth 2 x)
                   (cdr (assoc 'href (nth 1 x)))))
           forum-list)))

(defun leo--print-translation (pairs)
  "Format and print translation PAIRS."
  (if (null pairs) nil
    (princ
     (concat
      (caar pairs)
      "\n   -> "
      (cdar pairs)
      "\n"))
    (leo--print-translation (cdr pairs))))

(defun leo--print-forums (forum-pairs)
  "Format and print translation FORUM-PAIRS."
  (if (null forum-pairs) nil
    (princ
     (concat
      (caar forum-pairs)
      "\n   -> "
      (concat "https://dict.leo.org" (cdar forum-pairs))
      "\n"))
    (leo--print-forums (cdr forum-pairs))))

(defun leo--open-translation-buffer (pairs forums)
  "Print translation PAIRS to temporary buffer."
  (with-output-to-temp-buffer " *leo*"
      (princ "SEARCH RESULTS:\n\n")
      (leo--print-translation pairs)
      (princ "\n\nFORUM RESULTS:\n\n")
      (leo--print-forums forums))
  (other-window 1))

(defun leo--translate (lang word)
  "Translate WORD from LANG to German."
  (let ((xml (leo--parse-xml
              (leo--generate-url lang word))))
    (leo--open-translation-buffer
     (leo--extract-translation-pairs xml)
     (leo--extract-forum-subject-link-pairs xml))))

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
