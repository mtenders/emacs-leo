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

(defface leo--heading-face
    '((t :inherit font-lock-function-name-face :weight bold))
    "Face used for POS headings.")

(defface leo--search-and-forum-face
    '((t :weight bold))
    "Face used for Search and Forum headings.")

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

(defun leo--xml-node-itself (parsed-xml)
  "Why I have to do this?"
  (car parsed-xml))

(defun leo--get-result-lang-pair (xml-node)
  (xml-get-attribute xml-node 'lp))

(defun leo--get-result-section-list (xml-node)
  (xml-get-children xml-node 'sectionlist))

(defun leo--get-result-sections-as-list (section-list)
  (xml-get-children (car section-list) 'section))

(defun leo--get-section-part-of-speech (section)
  (xml-get-attribute section 'sctTitle))

;; a section also has attrs: number, short name, count

(defun leo--get-entries-from-section (section)
  (xml-get-children section 'entry))

(defun leo--get-info-from-entry (entry)
  (xml-get-children entry 'info))

(defun leo--get-entry-part-of-speech (entry)
  (let ((cat (xml-get-children (car (leo--get-info-from-entry entry))
                               'category)))
    (xml-get-attribute
     (car cat)
     'type)))

(defun leo--get-sides-from-entry (entry)
  (xml-get-children entry 'side))

(defun leo--get-lang-from-side (side)
  "Get the language that SIDE is in."
  (xml-get-attribute side 'lang))

(defun leo--get-child (tag child)
    "Why do I fucking have to do this?"
    (car (xml-get-children tag child)))

(defun leo--get-words-node-from-side (side)
  (xml-get-children side 'words))

(defun leo--extract-word-strings-as-list (words-node)
  ;; we collect a list to capture spelling variants, noun phrases, etc.
  (let ((word-node-list (xml-get-children (car words-node) 'word)))
    (mapcar (lambda (x)
              (car (xml-node-children x)))
            word-node-list)))

(defun leo--extract-phrases-from-words-list (words)
  "Returns a list of term plus any phrasal suffixes or spelling variants from an side's WORDS."
  ;;("(dependancy)" "(on)" "(dependancy on)" "(dependency)" "(on)")
  ;; is what it does for combo noun phr + sp variant
  ;; this wd work if you remove any list entries that = term also.
  ;; else try to make regex match EITHER following space OR end of str
  (let* ((term (concat (car words) " "))
         (suffix-list
          (mapcar (lambda (x)
                    (concat "("
                            (replace-regexp-in-string term "" x)
                            ")"))
                  words)))
    (list (cons 'term term)
          (cons 'suffixes (cadr suffix-list)))))

(defun leo--extract-words-from-side (side)
  (leo--extract-phrases-from-words-list
   (leo--extract-word-strings-as-list
    (leo--get-words-node-from-side side))))

;; ONLY FOR VERBS & PREPS
;; FAILS FOR PREPS, so have to key in through "entry" in order to test POS
;; for now just OR the two nodes: "i" / "sup"
;; NB: this adds more info to EN entries also.
;; some of which is useless eg terms announcing alternatives: "also,", "or:", "infinitive:", "pl.:", etc. but the alternative itself does not appear.
;; some are v useful tho:  (used with pl. verb) etc.
;; those accidentally caught markers need their own fun?
;; i put them in correct order by making cases precede suffixes, hack workaround for now.
;; cd search for only a specific subset to make precede suffixes also
(defun leo--extract-cases-from-side (side)
  "Extract a term's case from a given SIDE.
A side is either the source or target result for a given search."
  (let* ((repr (xml-get-children side 'repr))
         (smalls (leo--map-get-children repr 'small))
         (sups-or-is (or (leo--map-get-children smalls 'sup)
                         (leo--map-get-children smalls 'i)))
         ;; (sups-or-is (leo--map-get-children smalls 'sup))
         (ms (leo--map-get-children sups-or-is 'm))
         (cases (leo--map-get-children ms 't)))
    (or (mapcar (lambda (x)
                  (caddr x))
                cases)
        ""))) ; handle no case markers

(defun leo--strip-trailing-period (string)
  "Remove trailing period from STRING."
  ;;TODO test if it actually ends with a period!
  ;; for now just run it on EN tags, which all seem to.
  ;; & DE case markers
  (substring string 0 -1))

(defun leo--extract-tag-from-side (side)
  "Extract a term's domain tag from a given SIDE.
A side is either the source or target result for a given search."
  (let* ((repr (leo--get-child side 'repr))
         (lang (leo--get-lang-from-side side)))
    (cond ((equal lang "en")
           (let* (;; key in to en xml tag:
	              (domain (leo--get-child repr 'domain))
                  (small (leo--get-child domain 'small))
                  (m (leo--get-child small 'm))
                  (tag (leo--get-child m 't))
                  (tag-string (caddr tag)))
             (if tag
                 (if (> (length tag-string) 3) ;3 letter tags have no period?
                     (leo--strip-trailing-period tag-string)
                   tag-string)
               ""))) ; no tag
          ((equal lang "de")
           (let* (;; key in to de xml tag:
                  (virr (leo--get-child repr 'virr))
                  (small (leo--get-child virr 'small))
                  (i (leo--get-child small 'i))
                  (m (leo--get-child i 'm))
                  (tag (leo--get-child m 't)))
             (or (caddr tag)
                  "")))))) ;handle no tag

(defun leo--extract-plural-from-side (side)
  "Extract a term's plural form from a given SIDE.
A side is either the source or target result for a given search.
Returns a string ."
  (let* ((repr (leo--get-child side 'repr))
         (lang (leo--get-lang-from-side side)))
    (cond ((equal lang "en")
           (let ((small (leo--get-child repr 'small)))
             (or (cadddr small)
                 "")))
          ((equal lang "de")
           (let* ((flecttabref (leo--get-child repr 'flecttabref))
                  (small (leo--get-child flecttabref 'small)))
             (or (cadddr small)
                 "")))))) ; handle no plural

;; EN nouns have these, at the least
(defun leo--extract-context-marker-from-side (side)
  "Extract a term's plural form from a given SIDE.
A side is either the source or target result for a given search.
Returns a string ."
  (let* ((repr (leo--get-child side 'repr))
         (lang (leo--get-lang-from-side side)))
    (if (equal lang "en")
        (let* ((small (leo--get-child repr 'small))
               (i (leo--get-child small 'i)))
          (if (stringp (caddr i)) ;in case its a bunch more XML instead
              (or (caddr i)
                  ""))))))
          ;; ((equal lang "de")
           ;; ""))))
           ;; (let* ((flecttabref (leo--get-child repr 'flecttabref))
                  ;; (small (leo--get-child flecttabref 'small)))
             ;; (or (cadddr small)
                 ;; "")))))) ; handle no plural

;; ONLY FOR (DE) NOUNS?
;; and SOME EN verbs...
(defun leo--extract-flextable-from-side (side)
  "Extract the link to a term's inflexion table from a given SIDE.
A side is either the source or target result for a given search.
Returns a string ."
  (let* ((base-url "https://dict.leo.org/pages/flecttab/flectionTable.php?kvz=")
         (ibox (car (xml-get-children side 'ibox)))
         (flexmain (car (xml-get-children ibox 'flexmain)))
         (url-suffix (cdr (assoc 'table (car (cdaddr flexmain))))))
    (if flexmain
        (or (concat base-url url-suffix)
            ""))))

(defun leo--build-sections-list (section-list)
  "Returns a list of sections, sorted by part of speech and containing entries."
  (let ((sections (xml-get-children section-list 'section)))
    (mapcar (lambda (x)
              (leo--build-section-from-entries x))
          sections)))

(defun leo--build-section-from-entries (section)
  (let ((section-pos (leo--get-section-part-of-speech section)))
    (list (cons section-pos
                (leo--build-list-of-entries (leo--get-entries-from-section section))))))

(defun leo--build-list-of-entries (entries)
  "Returns a list of ENTRIES.
Each contains two sides, or results in a pair of languages."
  (mapcar (lambda (x)
            (leo--build-entry-from-sides x))
          entries))

(defun leo--build-entry-from-sides (entry)
  "Build an entry, ie a list of two sides."
  (let ((sides (leo--get-sides-from-entry entry)))
    (mapcar (lambda (x)
              (leo--build-side-from-words-and-elements x))
              sides)))

;; TODO: only search for things if the POS has those things
;; but POS is in ENTRY not SIDE
;; re-write for an entry and map the list-building
(defun leo--build-side-from-words-and-elements (side)
  "Construct a list for SIDE.
Each side contains the list of words, plus any plural forms, tags, case markers, etc."
  (list (leo--extract-words-from-side side)
        (cons 'pl (leo--extract-plural-from-side side))
        (cons 'tag (leo--extract-tag-from-side side))
        (cons 'case (leo--extract-cases-from-side side))
        (cons 'context (leo--extract-context-marker-from-side side))
        (cons 'table (leo--extract-flextable-from-side side))))

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

(defun leo--print-single-side (side)
  (let ((term (cdr (assoc 'term (car side))))
        (suffixes (cdr (assoc 'suffixes (car side))))
        (plural (cdr (assoc 'pl (cdr side))))
        (tag (cdr (assoc 'tag (cdr side))))
        (case-marks (cdr (assoc 'case (cdr side))))
        (context (cdr (assoc 'context (cdr side))))
        (table (cdr (assoc 'table (cdr side)))))
    (insert
     (concat
      (if table
          (propertize term
                      'button t
                      'follow-link t
                      'shr-url table
                      'keymap shr-map
                      'fontified t
                      'face 'leo--link-face
                      'mouse-face 'highlight
                      'help-echo (concat "Browse inflexion table for '"
                                         term "'"))
        term)
         (if (not (eq case-marks ""))
          (propertize (concat "("
                              (mapconcat #'identity case-marks ",")
                              ") ")
                      'face 'leo--auxiliary-face))
      (if suffixes
          (propertize suffixes
                      'face 'leo--auxiliary-face))
      (if (not (eq plural ""))
          (propertize plural
                      'button t
                      'follow-link t
                      'shr-url table
                      'keymap shr-map
                      'fontified t
                      'face 'leo--auxiliary-face
                      'mouse-face 'highlight
                      'help-echo (concat "Browse inflexion table for '"
                                         term "'")))
      (if (not (eq tag ""))
          (propertize (concat "[" tag "]")
                      'face 'leo--auxiliary-face))
      (if (not (eq context ""))
          (if (stringp context)
              (propertize (concat "(" context ")")
                          'face 'leo--auxiliary-face)))

      ;; (if table
          ;; (concat (propertize" | " 'face 'leo--auxiliary-face)
                  ;; (propertize "table"
                      ;; 'button t
                      ;; 'follow-link t
                      ;; 'shr-url table
                      ;; 'keymap shr-map
                      ;; 'fontified t
                      ;; 'face 'leo--auxiliary-face
                      ;; 'mouse-face 'highlight
                      ;; 'help-echo (concat "Browse inflexion table for '"
                                         ;; term "'"))))
      ))))

(defun leo--print-single-entry (entry)
  (leo--print-single-side (car entry))
  (insert
   (concat
    "\n           "
    (propertize "--> "
                'face 'leo--auxiliary-face)))
  (leo--print-single-side (cadr entry))
  (insert "\n\n"))

(defun leo--print-single-section (section)
  (let ((section-pos (caar section))
        (section-entries (cdar section)))
    (insert
     (propertize section-pos
                 'face 'leo--heading-face)
     "\n\n")
    (mapcar (lambda (x)
              (leo--print-single-entry x))
            section-entries)))

(defun leo--print-translation (results)
  "Format and print translation PAIRS.
Results include domain tags and plural forms."
  (if (null results) nil
    (with-current-buffer (get-buffer " *leo*")
      (mapcar (lambda (x)
                (leo--print-single-section x))
              results))))

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

(defun leo--open-translation-buffer (results forums word)
  "Print translation PAIRS and FORUMS in temporary buffer.
The search term WORD is propertized in results."
  (with-output-to-temp-buffer " *leo*"
    (princ (concat "leo.de search results for " word ":\n\n"))
    (leo--print-translation results)
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
  (let* ((xml (leo--parse-xml
              (leo--generate-url lang word)))
         (section-list (car (leo--get-result-section-list (car xml)))))
    (leo--open-translation-buffer
     (leo--build-sections-list section-list)
     ;; (leo--extract-translation-pairs xml)
     (leo--extract-forum-subject-link-pairs xml)
     word)))

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
