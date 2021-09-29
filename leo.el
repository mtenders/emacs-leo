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

(defgroup leo nil
  "Leo dictionary interface."
  :group 'leo)

(defcustom leo-language "en"
  "Language to translate from to German.

Available languages: en, es, fr, it, ch, pt, ru, pl"
  :type 'string
  :group 'leo
  :options '("es" "fr" "it" "ch" "pt" "ru" "pl"))

(defcustom leo-browse-url-function nil
  "The browser that is used to access online dictionaries."
  :group 'leo
  :type '(choice
          (const         :tag "Default" :value nil)
          (function-item :tag "Emacs W3" :value  browse-url-w3)
          (function-item :tag "W3 in another Emacs via `gnudoit'"
                         :value  browse-url-w3-gnudoit)
          (function-item :tag "Mozilla" :value  browse-url-mozilla)
          (function-item :tag "Firefox" :value browse-url-firefox)
          (function-item :tag "Chromium" :value browse-url-chromium)
          (function-item :tag "Galeon" :value  browse-url-galeon)
          (function-item :tag "Epiphany" :value  browse-url-epiphany)
          (function-item :tag "Netscape" :value  browse-url-netscape)
          (function-item :tag "eww" :value  eww-browse-url)
          (function-item :tag "Text browser in an xterm window"
                         :value browse-url-text-xterm)
          (function-item :tag "Text browser in an Emacs window"
                         :value browse-url-text-emacs)
          (function-item :tag "KDE" :value browse-url-kde)
          (function-item :tag "Elinks" :value browse-url-elinks)
          (function-item :tag "Specified by `Browse Url Generic Program'"
                         :value browse-url-generic)
          (function-item :tag "Default Windows browser"
                         :value browse-url-default-windows-browser)
          (function-item :tag "Default Mac OS X browser"
                         :value browse-url-default-macosx-browser)
          (function-item :tag "GNOME invoking Mozilla"
                         :value browse-url-gnome-moz)
          (function-item :tag "Default browser"
                         :value browse-url-default-browser)
          (function      :tag "Your own function")
          (alist         :tag "Regexp/function association list"
                         :key-type regexp :value-type function)))


(defface leo-link-face
  '((t :inherit warning))
  "Face used for links to forum posts."
  :group 'leo)

(defface leo-auxiliary-face
  '((t :inherit font-lock-comment-face))
  "Face used to fade auxiliary items."
  :group 'leo)

(defface leo-heading-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face used for POS headings."
  :group 'leo)

(defface leo-search-and-forum-face
  '((t :weight bold))
  "Face used for Search and Forum headings."
  :group 'leo)

(defface leo-match-face
  '((t :inherit success :weight bold))
  "Face used for search terms in search results."
  :group 'leo)

(defface leo-case-and-variant-marker-face
  '((t :inherit font-lock-comment-face :slant italic :height 0.8))
  "Face used to fade and italicise language case markers and variant markers in results."
  :group 'leo)

(defvar leo-result-search-map
  (let ((map (make-sparse-keymap)))
    ;; (let ((map (copy-keymap shr-map)))
    (define-key map [mouse-2] 'leo--translate-word-click-search)
    (define-key map (kbd "RET") 'leo--translate-word-return-search)
    map))

(defvar leo-inflexion-table-map
  (let ((map (make-sparse-keymap)))
    ;; (let ((map (copy-keymap shr-map)))
    (define-key map [mouse-2] 'shr-browse-url)
    (define-key map (kbd "RET") 'shr-browse-url)
    map))

(defvar leo-languages-full
  '(("en" . "englisch")
    ("es" . "spanisch")
    ("fr" . "französisch")
    ("it" . "italienisch")
    ("ch" . "chinesisch")
    ("pt" . "portugiesisch")
    ("ru" . "russisch")
    ("pl" . "polnisch"))
    "An alist linking language abbreviations to the full name.
Used to build the URL for external browsing to leo.de.")

(defvar leo--results-info nil
  "Information about the current results from a leo search. Used to store search term for `leo-browse-url-results'.")
(make-variable-buffer-local 'leo--results-info)

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

(defun leo--map-get-children (seq child)
  "Map xml-get-children over SEQ for CHILD."
  (mapcan
   (lambda (node) (xml-get-children node child))
   seq))

(defun leo--get-result-lang-pair (xml-node)
  (xml-get-attribute xml-node 'lp))

(defun leo--get-result-similar-list (xml-node)
  (xml-get-children xml-node 'similar))

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

(defun leo--get-words-node-from-side (side)
  (xml-get-children side 'words))

(defun leo--get-repr-children-strings-as-string (side)
  (dom-texts (dom-child-by-tag side 'repr) ""))

(defun leo--strip-redundant-scores (string)
  (replace-regexp-in-string "[ ]+" "" string))

(defun leo--get-repr-children-strings-as-string-trimmed (side)
  (leo--strip-redundant-scores
   (leo--get-repr-children-strings-as-string side)))

(defun leo--extract-word-strings-as-list (words-node)
   (let ((word-node-list (xml-get-children (car words-node) 'word)))
     (mapcar (lambda (x)
               (car (xml-node-children x)))
             word-node-list)))

(defun leo--strip-trailing-period (string)
  "Remove trailing period from STRING if it has one."
  (if (string-match "\\.$" string)
      (substring string 0 -1)
    string))

(defun leo--strip-redundant-parens (string)
  "Remove redundant ( ) from from STRING if it has them."
  (if (string-match "^(" string)
      (substring string 1 -1)
    string))

(defun leo--extract-flextable-from-side (side)
  "Extract the link to a term's inflexion table from a given SIDE.
A side is either the source or target result for a given search.
Returns a string ."
  (let* ((base-url "https://dict.leo.org/pages/flecttab/flectionTable.php?kvz=")
         (ibox (car (xml-get-children side 'ibox)))
         (flexmain (car (xml-get-children ibox 'flexmain)))
         (url-suffix (cdr (assoc 'table (car (cdaddr flexmain))))))
    (if flexmain
        (concat base-url url-suffix))))

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

;;; BUILDING RESULTS LISTS
(defun leo--build-sections-list (section-list)
  "Return a list of sections from parsed XML SECTION-LIST.
Sections are contain entries for a single part of speech."
  (let ((sections (xml-get-children section-list 'section)))
    (mapcar (lambda (x)
              (leo--build-section-from-entries x))
          sections)))

(defun leo--build-section-from-entries (section)
  "Build a SECTION, or group of entries all of the one part of speech."
  (let ((section-pos (leo--get-section-part-of-speech section)))
    (list (cons section-pos
                (leo--build-list-of-entries (leo--get-entries-from-section section))))))

(defun leo--build-list-of-entries (entries)
  "Return a list of ENTRIES.
Each contains two sides, or results in a pair of languages."
  (mapcar (lambda (x)
            (leo--build-entry-from-sides x))
          entries))

(defun leo--build-entry-from-sides (entry)
  "Build an ENTRY, ie a list of two sides."
  (let ((sides (leo--get-sides-from-entry entry)))
    (mapcar (lambda (x)
              (list
               (cons 'words-list (leo--extract-word-strings-as-list
                            (leo--get-words-node-from-side x)))
               (cons 'result (leo--get-repr-children-strings-as-string-trimmed x))
               (cons 'table (leo--extract-flextable-from-side x))))
            sides)))

(defun leo--add-props-to-match (match)
  "Add text properties to string MATCH, including TERM."
  (add-text-properties (match-beginning 0) (match-end 0)
                       (list 'button t
                             'follow-link t
                             'keymap leo-result-search-map
                             'fontified t
                             'face 'leo-link-face
                             'mouse-face 'highlight
                             'help-echo (concat
                                         "Click to search leo for this term"))
                       match))

(defun leo--propertize-past-participles (result)
  "Set the properties of past participles in RESULT to `leo-auxiliary-face'."
  (save-match-data
    (when (string-match "|[ a-z,/]+,+[ a-z,/]+|" result)
      ;; if we have past participles; regex tries to mandate a comma to
      ;; differentiate this from DE adj. that also use "|"
      (set-text-properties (match-beginning 0) (match-end 0)
                           (list 'face 'leo-auxiliary-face)
                           result))
    result))

(defun leo--add-term-prop-to-match (match term)
  "Add text property 'term TERM to string MATCH."
  (add-text-properties (match-beginning 0) (match-end 0)
                       (list 'term term)
                       match))

(defun leo--add-ital-prop-to-case-and-variant-marker (match)
  "Add text property ``leo-case-and-variant-marker-face' to string MATCH."
  (set-text-properties (match-beginning 0) (match-end 0)
                       (list 'face 'leo-case-and-variant-marker-face)
                       match))

(defun leo--propertize-variant-markers-in-result (result)
  "Add property `leo-case-and-variant-marker-face' to variant markers in RESULT."
  (let ((v-marker '("BE" "AE" "espAE" "espBE"))
        (case-fold-search nil)) ; case-sensitive matching
    (save-match-data
      (mapc (lambda (x)
                (if (string-match x result)
                    (leo--add-ital-prop-to-case-and-variant-marker result))
                ;; match again starting from end of prev match
                (if (string-match x result (match-end 0))
                    (leo--add-ital-prop-to-case-and-variant-marker result)))
              v-marker)))
  result)

(defun leo--propertize-case-markers-in-result (result)
  "Add property `leo-case-and-variant-marker-face' to case markers in RESULT."
  (let ((c-marker '("Dat." "Nom." "Gen." "Akk."))
        (case-fold-search nil)) ; case-sensitive matching
    (save-match-data
      (mapc (lambda (x)
                (if (string-match x result)
                    (leo--add-ital-prop-to-case-and-variant-marker result))
                ;; match again starting from end of prev match
                (if (string-match x result (match-end 0))
                    (leo--add-ital-prop-to-case-and-variant-marker result)))
              c-marker)))
  result)

(defun leo--space-before-term (leo-words-list result)
  "Ensure a space before any words in LEO-WORDS-LIST in string RESULT.
This is to handle the loss of our <br> tags in the XML."
  ;; needs to work on second variant, not on first item in words-list
  (let ((result result))
    (save-match-data
      (while leo-words-list
        (let ((term (car leo-words-list)))
          ;; if term preceded by neither space nor newline
          (when (string-match (concat "[^[:blank:]\n]"
                                      term)
                              result)
            (setq result (replace-match
                          ;; regex matches preceding car so we get it
                          (concat (substring (match-string 0 result) 0 1)
                                  ;; then a space
                                  " "
                                  ;; then our term
                                  (substring (match-string 0 result) 1 nil))
                          t nil result))))
        (setq leo-words-list (cdr leo-words-list))))
    result))

(defun leo--propertize-words-list-in-result (result leo-words-list)
  "Add properties to words in RESULT that match words in LEO-WORDS-LIST.
List items in words-list are applied as both split lists and whole strings."
  (let ((has-variants-p (if (or (string-match "BE" result)
                                (string-match "AE" result)
                                (string-match "espAE" result)
                                (string-match "espBE" result))
                            t))
        (has-cases-p (if (or (string-match "Nom." result)
                             (string-match "Akk." result)
                             (string-match "Dat." result)
                             (string-match "Gen." result))
                         t))
        (leo-last-match-end))
    (while leo-words-list
      (let* ((term (car leo-words-list))
             (term-spl (split-string term)))
        (save-match-data
          (if (string-match term result leo-last-match-end) ; start from last match
              ;; try to match and propertize full term first:
              ;; this avoids making each word in term a separate tab stop
              (progn
                ;; add properties to whole term string (for tab stops):
                (leo--add-props-to-match result)
                ;; add term property separately to each word in term list
                ;; for click to search each word separately, not whole term string:
                (mapc (lambda (x)
                          (string-match x result leo-last-match-end)
                          (leo--add-term-prop-to-match result x))
                        term-spl)
                ;; this presumes any repetion comes after not before any variant
                (setq leo-last-match-end (match-end 0)))
            ;; ELSE match each word in term separately:
            (mapc (lambda (x)
                      (if (string-match x result)
                          (progn
                            (leo--add-props-to-match result)
                            (leo--add-term-prop-to-match result x)))
                      ;; match again starting at end of prev match
                      (if has-variants-p ; only run on variants
                          (if (string-match x result (match-end 0))
                              (progn
                                (leo--add-props-to-match result)
                                (leo--add-term-prop-to-match result x)))))
                    term-spl))))
      (setq leo-words-list (cdr leo-words-list)))
    (when has-variants-p ; only run on variants
        (leo--propertize-variant-markers-in-result result))
    (when has-cases-p
      (leo--propertize-case-markers-in-result result))
    ;; handle any accidental propertizing of past participles:
    (leo--propertize-past-participles result)
    result))

;;; PRINTING
(defun leo--print-single-side (side)
  "Print a single SIDE of a result entry."
  (let* ((words-list (cdr (assoc 'words-list side)))
         (result-no-prop (cdr (assoc 'result side)))
         (result-prop (propertize result-no-prop
                                  'face 'leo-auxiliary-face))
         (result (leo--space-before-term words-list result-prop))
         (table (cdr (assoc 'table side))))
    (insert
     (concat
      (if table
          (concat
           (propertize
            (if (fontp (char-displayable-p #10r9638))
                "▦"
              "#")
            'button t
            'follow-link t
            'shr-url table
            'keymap leo-inflexion-table-map
            'fontified t
            'face 'leo-auxiliary-face
            'mouse-face 'highlight
            'help-echo (concat "Browse inflexion table for '"
                               (car words-list) "'"))
           " "))
      (when result
            (leo--propertize-words-list-in-result result words-list))))))

(defun leo--print-single-entry (entry)
  "Print an ENTRY, consisting of two sides of a result."
  (leo--print-single-side (car entry))
  (insert
   (concat
    "\n           "
    (propertize "--> "
                'face 'leo-auxiliary-face)))
  (leo--print-single-side (cadr entry))
  (insert "\n\n"))

(defun leo--print-single-section (section)
  "Print a single result SECTION, which is one side of an entry."
  (let ((section-pos (caar section))
        (section-entries (cdar section)))
    (insert
     (propertize section-pos
                 'face 'leo-heading-face)
     "\n\n")
    (mapcar (lambda (x)
              (leo--print-single-entry x))
            section-entries)))

(defun leo--print-translation (results word similar)
  "Format and print translation RESULTS.
WORD is the search term, SIMILAR is a list of suggestions to display if results are nil."
    (with-current-buffer (get-buffer " *leo*")
      (if (null results) ;nil
          (leo--did-you-mean word similar)
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
                        'face 'leo-link-face
                        'mouse-face 'highlight
                        'help-echo (concat "Browse forum entry for '"
                                           (caar forum-posts) "'")))
           (teaser
            (propertize (nth 2 (car forum-posts))
                        'face 'leo-auxiliary-face)))
      (with-current-buffer (get-buffer " *leo*")
        (insert
         (concat
          post-title
          "\n"
          teaser
          "\n\n"
          (leo--print-forums (cdr forum-posts))))))))

(defun leo-browse-url-results ()
  "Open the current results for LANG and WORD in external browser.
Uses `leo-browse-url-function' to decide which browser to use."
  (interactive)
  (let* ((lang-full (cdr (assoc leo-language leo-languages-full)))
         (word (plist-get leo--results-info 'term))
         (search-url (concat "https://dict.leo.org/" lang-full "-deutsch/" word))
         (browse-url-browser-function (or leo-browse-url-function
                                          (if (browse-url-can-use-xdg-open)
                                              (browse-url-xdg-open search-url))
                                          browse-url-secondary-browser-function
                                          browse-url-browser-function)))
    (browse-url search-url)))

(defun leo--search-term-with-dictcc ()
  "Repeat current search with dict.cc."
  (interactive)
  (dictcc (plist-get leo--results-info 'term)))

(defun leo--translate-word-click-search (event)
  "Translate word on mouse click EVENT from language set by 'leo-language' to German."
  (interactive "e")
  (leo--translate leo-language (get-text-property (point) 'term )))

(defun leo--translate-word-return-search ()
  "Translate word or phrase at point between `leo-language' and German.
Word or phrase at point is determined by button text property."
  (interactive)
  (let ((text (buffer-substring-no-properties
               (progn
                 (if (looking-back "[ \t\n]" nil) ; enter range if we only tabbed here
                     (forward-char))
                 (previous-single-property-change (point) 'button)) ; get range start
               (next-single-property-change (point) 'button))))
    (leo--translate leo-language text)))

(defun leo--did-you-mean (word similar)
  "Print some alternative terms SIMILAR to search for.
Used if `leo--print-translation' for WORD has no results.
Results are links to searches for themselves."
  (let* ((sim-sides (xml-get-children similar 'side))
         (sim-word-nodes (leo--map-get-children sim-sides 'word))
         (sim-word-strings
          (mapcar (lambda (x)
                    (car (xml-node-children x)))
                  sim-word-nodes))
         (sim-words-propertized
          (mapcar (lambda (x)
                      (propertize x
                                  'button t
                                  'follow-link t
                                  'term x
                                  'keymap leo-result-search-map
                                  'fontified t
                                  'face 'leo-link-face
                                  'mouse-face 'highlight
                                  'help-echo (concat "Search leo for '"
                                                     x "'")))
                  sim-word-strings)))
    (insert
     (concat
      "No entries for " word ". "
      (if sim-words-propertized
          (concat
          "Did you mean:\n\n "
          (mapconcat #'identity sim-words-propertized "  ")))
      "\n\nHit 't' to search again.\n\n"))))

(defun leo--make-buttons ()
  "Make all property ranges with button property into buttons."
  (with-current-buffer (get-buffer " *leo*")
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (next-single-property-change (point) 'button)
          (make-text-button
           (goto-char (next-single-property-change (point) 'button))
           (goto-char (next-single-property-change (point) 'button))))))))

(defun leo--propertize-search-term-in-results (word)
  "Add `leo--match-face' to any instances of WORD in results buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp word nil 'noerror)
        ;; (let ((props (text-properties-at (- (point) 1))))
          ;; (remove-text-properties (- (point) (length word)) (point)
                                  ;; '(face face))
          (add-text-properties (- (point) (length word)) (point)
                               '(face leo-match-face))))))

(defun leo--print-results-buffer-heading (word)
  "Insert heading in buffer showing results for WORD."
  (with-current-buffer (get-buffer " *leo*")
    (insert
     (propertize
      (concat "leo.de search results for " word ":\n\n")
      'face 'leo-search-and-forum-face))))

(defun leo--print-results-buffer-forum-heading (word)
  "Insert forum results heading in buffer showing results for WORD."
  (with-current-buffer (get-buffer " *leo*")
    (insert
     (propertize
      (concat "leo.de forum results for " word ":\n\n")
      'face 'leo-search-and-forum-face))))

(defun leo--open-translation-buffer (results forums word similar)
  "Print translation RESULTS and FORUMS in temporary buffer.
The search term WORD is propertized in results.
SIMILAR is a list of suggestions to display if there are no results."
  (with-output-to-temp-buffer " *leo*" ; this makes it help-mode
    (leo--print-results-buffer-heading word)
    (leo--print-translation results word similar)
    (leo--print-results-buffer-forum-heading word)
    (leo--print-forums forums))
  ;; make rly sure we are in correct buffer
  ;; before we set do any keymapping
  (with-current-buffer (get-buffer " *leo*")
    (leo--make-buttons)
    (leo--propertize-search-term-in-results word)
    ;; hack to not ruin help-mode bindings, till we have a minor mode:
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key (kbd "t") 'leo-translate-word)
    (local-set-key (kbd "b") 'leo-browse-url-results)
    (when (require 'dictcc nil :noerror)
      (local-set-key (kbd "c") 'leo--search-term-with-dictcc))
    (setq leo--results-info `(term ,word)))
  (if (not (equal (buffer-name (current-buffer)) " *leo*"))
      (other-window 1)))

(defun leo--translate (lang word)
  "Translate WORD from LANG to German."
  (let* ((xml (leo--parse-xml
              (leo--generate-url lang word)))
         (section-list (car (leo--get-result-section-list (car xml))))
         ;; similar terms to offer if no results:
         (similar-list (car (leo--get-result-similar-list (car xml)))))
    (leo--open-translation-buffer
     (leo--build-sections-list section-list)
     (leo--extract-forum-subject-link-pairs xml)
     word
     similar-list)))

;;;###autoload
(defun leo-translate-word (word &optional lang)
  "Translate WORD between language set by 'leo-language' and German.
Show translations in new buffer window.
Optional prefix argument LANG prompts to set language for this search."
  (interactive "sTranslate: \nP")
  (let* ((language-candidates
          ;; transpose alist for comp read to display full lang name
          (mapcar (lambda (x)
                    (cons (cdr x) (car x)))
                  leo-languages-full)))
    (if current-prefix-arg
        ;; if prefix: prompt for language to search for:
        (let ((lang (completing-read
                     (format "Language (to pair with German) (%s): "
                             (car (rassoc leo-language language-candidates)))
                     language-candidates nil t nil nil
                     (rassoc leo-language language-candidates))))
          (leo--translate (cdr (assoc lang language-candidates))
                          word))
      ;; else normal search:
      (leo--translate leo-language word)))
  (message (concat "'t' to search again, prefix to choose language, 'b' to view in browser"
                   (when (require 'dictcc nil :noerror)
                     ", 'c' to search with dictcc.el"))))

;;;###autoload
(defun leo-translate-at-point (&optional lang)
  "Translate word under cursor between `leo-language' and German.
Show translations in new buffer window.
Optional prefix argument LANG prompts to set language for this search."
  (interactive "P")
  (let* ((language-candidates
          ;; transpose alist so comp read displays full lang name
          (mapcar (lambda (x)
                    (cons (cdr x) (car x)))
                  leo-languages-full)))
    (if current-prefix-arg
        ;; if prefix: prompt for language to search for:
        (let ((lang (completing-read
                     (format "Language (to pair with German) (%s): "
                             (car (rassoc leo-language language-candidates)))
                     language-candidates nil t nil nil
                     (rassoc leo-language language-candidates))))
          (leo--translate (cdr (assoc lang language-candidates))
                          (current-word)))
      ;; else normal search:
      (leo--translate leo-language (current-word))))
  (message (concat "'t' to search again, prefix to choose language, 'b' to view in browser"
                   (when (require 'dictcc nil :noerror)
                     ", 'c' to search with dictcc.el"))))


(provide 'leo)
;;; leo.el ends here
