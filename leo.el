;;; leo.el --- Interface for dict.leo.org -*- lexical-binding:t -*-
;;
;; Copyright (C) 2020 M.T. Enders <michael AT michael-enders.com>
;;               2021 Marty Hiatt <martianhiatus AT riseup.net>
;;
;; Author: M.T. Enders <michael AT michael-enders.com>
;;         Marty Hiatt <martianhiatus AT riseup.net>
;; Created: 21 Oct 2020
;;
;; Package-Requires: ((emacs "27.1") (s "1.12.0"))
;; Keywords: convenience, translate
;; URL: https://github.com/mtenders/emacs-leo
;; Version: 0.3
;; Prefix: leo
;; Separator: -

;;; Commentary:
;;
;; A simple interface for dict.leo.org.
;;
;; Usage:
;;
;; This provides the commands leo-translate-word and
;; leo-translate-at-point. Both translate between the language set by
;; the custom variable leo-language, or chosen interactively if called
;; with a prefix argument, and German.
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
(require 's)
(require 'xml)
(require 'dom)
(require 'shr)
(require 'browse-url)
(require 'url-cache)
(require 'text-property-search)
(when (require 'dictcc nil :noerror)
  (declare-function dictcc "dictcc"))

(when (require 'helm-dictionary nil :noerror)
  (declare-function helm-dictionary "helm-dictionary")
  (defvar helm-dictionary-database)
  (defvar leo-helm-dictionary-name "de-en"
    "The name of the dictionary to use for `helm-dictionary' queries.\
It must match the key of one of the dictionaries in `helm-dictionary-database'."))

(when (require 'pdf-tools nil :no-error)
  (declare-function pdf-view-active-region-text "pdf-view"))

(defvar url-user-agent)

(defgroup leo nil
  "Leo dictionary interface."
  :group 'leo)

(defcustom leo-language "en"
  "Language to translate from to German.

Available languages: en, es, fr, it, ch, pt, ru, pl"
  :type 'string
  :group 'leo
  :options '("es" "fr" "it" "ch" "pt" "ru" "pl"))

(defcustom leo-user-agent url-user-agent
  "The user agent to send with requests to the Leo server.

The default is the current `url-user-agent' setting. It can be
manually set, or if set to default, can itself be customized
using `url-privacy-level'. Other option is to use the Tor user
agent."
  :group 'leo
  :type '(choice
          (function-item :tag "Default" :value url-user-agent)
          (function-item :tag "Tor" :value
                         "Mozilla/5.0 (Windows NT 10.0; rv:78.0) Gecko/20100101 Firefox/78.0")))

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
  "Face used to fade and italicise language case markers and
variant markers in results."
  :group 'leo)

(defvar leo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "t") #'leo-translate-word)
    (define-key map (kbd "s") #'leo-translate-word)
    (define-key map (kbd "b") #'leo-browse-url-results)
    (define-key map (kbd ",") #'leo-previous-heading)
    (define-key map (kbd ".") #'leo-next-heading)
    (define-key map (kbd "f") #'leo-jump-to-forum-results)
    (define-key map (kbd "<") #'leo-translate-left-side-only)
    (define-key map (kbd ">") #'leo-translate-right-side-only)
    (when (require 'dictcc nil :noerror)
      (define-key map (kbd "c") #'leo--search-term-with-dictcc))
    (define-key map (kbd "l") #'leo-browse-url-linguee)
    (when (require 'helm-dictionary nil :noerror)
      (define-key map (kbd "d") #'leo-search-in-helm-dictionary-de))
    map)
  "Keymap for leo mode.")

(defvar leo-result-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'leo--translate-word-click-search)
    (define-key map (kbd "RET") #'leo--translate-word-return-search)
    map))

(defvar leo-result-heading-search-more-pos-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'leo--translate-word-click-search-more-pos)
    (define-key map (kbd "RET") #'leo--translate-word-return-search-more-pos)
    map))

(defvar leo-inflexion-table-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'leo-shr-browse-url-secondary)
    (define-key map (kbd "RET") #'leo-shr-browse-url-secondary)
    map))

(defvar leo-forums-map
  (let ((map (copy-keymap shr-map)))
    (define-key map [mouse-2] #'leo-shr-browse-url-secondary)
    (define-key map (kbd "RET") #'leo-shr-browse-url-secondary)
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
Used to build the URL for external browsing to leo.de.
And, reversed, to prompt for language choice when `leo-translate-word'
and `leo-translate-at-point' are called with a prefix arg.")

(defvar leo--results-info nil
  "Information about the current results from a leo search.
Used to store search term for `leo-browse-url-results', and
language searched for `leo--translate-word-return-search' or
`leo--translate-word-click-search' after `leo-translate-word'
is called with a prefix argument to set a non-default search
language.")
(make-variable-buffer-local 'leo--results-info)

(defun leo--generate-url (lang word &optional pos side)
  "Generate link to query for translations of WORD from LANG to German.
Returns 16 results per POS."
  (let* ((pos-list '(("Substantive" . "1")
                     ("Verben" . "2")
                     ("Adjektive/Adverbien" . "3")
                     ("Präpositionen/Pronomen" . "4")))
         (pos-as-num (cdr (assoc pos pos-list))))
    (concat
     "https://dict.leo.org/dictQuery/m-vocab/"
     lang
     "de/query.xml?tolerMode=nof&lp="
     lang
     "de&lang=de&rmWords=off&rmSearch=on&search="
     word
     "&side="
     (or side
         "both")
     "&order=basic&partial=show"
     (when pos
       (concat "&fixedSect="
               pos-as-num
               "&sectLenMax=30"))
     "&multiwordShowSingle=on")))

(defun leo--parse-xml (url)
  "Parse xml file retrieved from URL."
  (let ((url-user-agent leo-user-agent))
    (if (url-is-cached url)
        (with-current-buffer (url-fetch-from-cache url)
          (set-buffer-multibyte nil)
          (let ((start (goto-char (re-search-forward "\n\n"))))
            (zlib-decompress-region start (point-max))
            (set-buffer-multibyte t)
            (xml-parse-region start (point-max))))
      (with-temp-buffer
	    (url-insert-file-contents url)
	    (xml-parse-region (point-min) (point-max))))))

(defun leo--map-get-children (seq child)
  "Map `xml-get-children' over SEQ for CHILD."
  (mapcan
   (lambda (node) (xml-get-children node child))
   seq))

(defun leo--get-result-lang-pair (xml-node)
  "Get the language pair attribute of an XML-NODE."
  (xml-get-attribute xml-node 'lp))

(defun leo--get-result-similar-list (xml-node)
  "Get the parsed XML of the list of similar terms from an XML-NODE."
  (xml-get-children xml-node 'similar))

(defun leo--get-result-section-list (xml-node)
  "Get the parsed XML of the section-list node of an XML-NODE."
  (xml-get-children xml-node 'sectionlist))

(defun leo--get-result-sections-as-list (section-list)
  "Get the parsed XML of section nodes from a SECTION-LIST node."
  (xml-get-children (car section-list) 'section))

(defun leo--get-section-part-of-speech (section)
  "Get the part of speech of a SECTION."
  (xml-get-attribute section 'sctTitle))

;; a section also has attrs: number, short name, count

(defun leo--get-entries-from-section (section)
  "Get the parsed XML of the entries from a SECTION."
  (xml-get-children section 'entry))

(defun leo--get-info-from-entry (entry)
  "Get the parsed XML of the info node of an ENTRY."
  (xml-get-children entry 'info))

(defun leo--get-entry-part-of-speech (entry)
  "Get the part of speech of an ENTRY."
  (let ((cat (xml-get-children
              (car (leo--get-info-from-entry entry)) 'category)))
    (xml-get-attribute (car cat) 'type)))

(defun leo--get-sides-from-entry (entry)
  "Get the parsed XML of the sides nodes from ENTRY."
  (xml-get-children entry 'side))

(defun leo--get-lang-from-side (side)
  "Get the language that SIDE is in."
  (xml-get-attribute side 'lang))

(defun leo--get-words-node-from-side (side)
  "Get the parsed XML of the <words> node in SIDE."
  (xml-get-children side 'words))

(defun leo--get-repr-children-strings-as-string (side)
  "Get the parsed XML of the childrend nodes of <repr> in SIDE."
  (dom-texts (dom-child-by-tag side 'repr) ""))

(defun leo--strip-redundant-scores (string)
  "Remove redundant underscores from STRING."
  (replace-regexp-in-string "[ ]+" "" string))

(defun leo--get-repr-children-strings-as-string-trimmed (side)
  "Get the parsed XML of the children nodes of <repr> in SIDE.
Remove redundant underscores from the result."
  (leo--strip-redundant-scores
   (leo--get-repr-children-strings-as-string side)))

(defun leo--extract-word-strings-as-list (words-node)
  "Make a list containing each entry in WORDS-NODE."
  (let ((word-node-list (xml-get-children (car words-node) 'word)))
    (mapcar (lambda (x)
              (car (xml-node-children x)))
            word-node-list)))

(defun leo--strip-trailing-period (string)
  "Remove trailing period from STRING if it has one."
  (if (string-match "\\.$" string)
      (substring string 0 -1)
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
                (leo--build-list-of-entries
                 (leo--get-entries-from-section section))))))

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
               (cons 'table (leo--extract-flextable-from-side x))
               (cons 'lang (leo--get-lang-from-side x))))
            sides)))

;; PROPERTIZING
(defun leo--add-props-to-match (match)
  "Add text properties to string MATCH."
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

(defun leo--add-term-prop-to-match (match term)
  "Add text property 'term TERM to string MATCH."
  (add-text-properties (match-beginning 0) (match-end 0)
                       (list 'term term)
                       match))

(defun leo--propertize-past-participles-in-result (result)
  "Set past participles in RESULT to `leo-auxiliary-face' only."
  (save-match-data
    (when (string-match "|[ a-z,/]+,+[ a-z,/]+|" result)
      ;; mandates a comma to differentiate this from DE adj. sets that
      ;; also use "|"
      (set-text-properties (match-beginning 0) (match-end 0)
                           (list 'face 'leo-auxiliary-face)
                           result))
    result))

(defun leo--case-and-variant-marker-face (match)
  "Add text property `leo-case-and-variant-marker-face' to string MATCH.
Also makes case and variant markers superscript unless preceded
by + or \\."
  (set-text-properties (match-beginning 0) (match-end 0)
                       (list 'face 'leo-case-and-variant-marker-face
                             'display
                             ;; only if not after a "+" or "/":
                             (when
                                 (with-temp-buffer
                                   (insert match)
                                   (goto-char (match-beginning 0))
                                   ;; match includes + so looking-at:
                                   (and (not (looking-at "+"))
                                        (not (looking-at "/"))))
                               '(raise 0.5)))
                       match))

(defun leo--propertize-case-or-variant-markers (markers result)
  "Add `leo-case-and-variant-marker-face' to MARKERS in RESULT."
  (let* ((case-fold-search nil))
    (save-match-data
      (mapc (lambda (x)
              (when (string-match x result)
                (leo--case-and-variant-marker-face result))
              ;; match again starting from end of prev match
              (when (string-match
                     x result
                     ;; only when result is longer than match-end
                     ;; should prevent previous match data being
                     ;; used that is longer than the result
                     (when (>= (length result) (match-end 0))
                       (match-end 0)))
                (leo--case-and-variant-marker-face result)))
            markers)))
  result)

(defun leo--space-before-term (leo-words-list result)
  "Ensure a space before any words in LEO-WORDS-LIST in string RESULT.
This is to handle the loss of our <br> tags in the XML."
  ;; needs to work on second variant, not on first item in words-list
  (let ((result result))
    (save-match-data
      (while leo-words-list
        (let ((term (car leo-words-list))
              (case-fold-search nil)) ; to match upper case "E"
          ;; if square bracket followed by a alphanum,
          ;; non-greedy one-or-more
          ;; shd match one char if matching
          (when (or (string-match "][[:alpha:]]+?" result)
                    ;; or match AE/BE + term with not space
                    (string-match (concat "E"
                                          (substring-no-properties
                                           ;; we don't care how it ends
                                           term 0 1)) ;2 breaks one-letter search
                                  result))
            (setq result (replace-match
                          ;; regex matches preceding char so we get it
                          (concat (substring (match-string 0 result) 0 1)
                                  " " ; then a space
                                  ;; then our term
                                  (substring (match-string 0 result) 1 nil))
                          t nil result))))
        (setq leo-words-list (cdr leo-words-list))))
    result))

(defun leo--propertize-words-list-in-result (result leo-words-list)
  "Add properties to words in RESULT that match words in LEO-WORDS-LIST.
List items in words-list are applied as both split lists and whole strings."
  (let ((leo-last-match-end)
        ;; ensure we match entries that have both
        ;; entries with only AE or BE don't need the doubling up
        (has-variants-p (if (or (and (string-match "BE" result)
                                     (string-match "AE" result))
                                (and (string-match "espAE" result)
                                     (string-match "espBE" result)))
                            t)))
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
            (let ((leo-last-match-end-split))
              (mapc (lambda (x)
                      (when (string-match x result leo-last-match-end-split)
                        (leo--add-props-to-match result)
                        (leo--add-term-prop-to-match result x)
                        (setq leo-last-match-end-split (match-end 0)))
                      ;; match again starting at end of prev match
                      (if has-variants-p ; only run on variants
                          (when (string-match x result (match-end 0))
                            (leo--add-props-to-match result)
                            (leo--add-term-prop-to-match result x))))
                    term-spl)))))
      (setq leo-words-list (cdr leo-words-list)))
    result))

(defun leo-has-markers-p (markers result)
  "Test if string RESULT contains any items in list MARKERS."
  (let ((marks-p)
        (case-fold-search nil))
    (dolist (marker markers)
      (if (string-match-p marker result)
          (setq marks-p t)))
    marks-p))

(defun leo--propertize-result-string (result leo-words-list)
  "Return a nicely formatted and propertized RESULT for printing a side.
LEO-WORDS-LIST is the list of words and phrases in <words>, which
will be propertized in result. POS is the part of speech of the
result."
  (let* ((cases '("Nom\\." "Akk\\." "Dat\\." "Gen\\."))
         (vars '("BE" "AE" "espAE" "espBE"))
         (has-cases-p (leo-has-markers-p cases result))
         (has-variants-p (leo-has-markers-p vars result))
         (result (leo--propertize-words-list-in-result
                  (s-collapse-whitespace
                   (leo--space-before-term leo-words-list
                                           (propertize
                                            result
                                            'face 'leo-auxiliary-face)))
                  leo-words-list)))
    (when has-variants-p
      (leo--propertize-case-or-variant-markers vars result))
    (when has-cases-p
      (leo--propertize-case-or-variant-markers cases result))
    ;; handle any accidental propertizing of past participles:
    (leo--propertize-past-participles-in-result result)
    result))

;;; PRINTING
(defun leo--print-single-side (side)
  "Print a single SIDE of a result entry.
POS is the part of speech of the side."
  (let* ((words-list (cdr (assoc 'words-list side)))
         (result (cdr (assoc 'result side)))
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
        (leo--propertize-result-string result words-list))))))

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
                 'button t
                 'follow-link t
                 'heading t
                 'face 'leo-heading-face
                 'mouse-face 'highlight
                 'keymap leo-result-heading-search-more-pos-map
                 'help-echo
                 (concat
                  "Click or RET to view more results for this part of speech"))
     "\n\n")
    (mapcar (lambda (x)
              (leo--print-single-entry x))
            section-entries)))

(defun leo--print-translation (results word similar)
  "Format and print translation RESULTS.
WORD is the search term, SIMILAR is a list of suggestions to
display if results are nil."
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
                        'keymap leo-forums-map
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
                                          (when (browse-url-can-use-xdg-open)
                                            '(browse-url-xdg-open))
                                          browse-url-secondary-browser-function
                                          browse-url-browser-function)))
    (browse-url search-url)))

(defun leo-shr-browse-url-secondary ()
  "Browse URL link at point using `browse-url-secondary-browser-function'.
\nI.e. usually an external browser. Used by `leo-forums-map' and
`leo-inflexion-table-map' to mandate external browser for those
types of links, as `shr-browse-url' only uses one when called
with a prefix arguemnt."
  (interactive)
  (let ((browse-url-browser-function browse-url-secondary-browser-function))
    (shr-browse-url)))

(defun leo--search-term-with-dictcc ()
  "Repeat current search with dict.cc."
  (interactive)
  (dictcc (plist-get leo--results-info 'term)))

(defun leo-browse-url-linguee ()
  "Search for current term in browser with Linguee.de."
  (interactive)
  (let* ((query (plist-get leo--results-info 'term))
         (query-split (split-string query " "))
         (query-final (if (not (> (length query-split) 1))
                          query
                        (string-join query-split "+"))))
    (browse-url-generic (concat
                         "https://www.linguee.de/deutsch-englisch/search?source=auto&query="
                         ;; "https://www.linguee.de/deutsch-englisch/uebersetzung/"
                         query-final))))
;; (plist-get leo--results-info 'term)
;; ".html"))))

(defun leo-search-in-helm-dictionary-de ()
  "Search for current query in `helm-dictionary'."
  (interactive)
  (let ((query (concat "\\b"
                       (plist-get leo--results-info 'term)
                       "\\b")))
    (helm-dictionary leo-helm-dictionary-name query t)))

(defun leo--translate-word-click-search (event)
  "Translate word on mouse click EVENT between `leo-language' and German."
  (interactive "e")
  (let ((lang (or (plist-get leo--results-info 'lang) ;stored prefix lang choice
                  leo-language))) ;fallback
    (leo--translate lang (get-text-property
                          (posn-point (event-end event))
                          'term))))

(defun leo--translate-word-return-search ()
  "Translate word or phrase at point between `leo-language' and German.
Word or phrase at point is determined by button text property."
  (interactive)
  (let ((lang (or (plist-get leo--results-info 'lang) ;stored prefix lang choice
                  leo-language)) ;fallback
        (text (buffer-substring-no-properties
               (progn
                 (if (looking-back "[ \t\n]" nil) ; enter range if we tabbed here
                     (forward-char))
                 (previous-single-property-change (point) 'button)) ; range start
               (next-single-property-change (point) 'button))))
    (leo--translate lang text)))

(defun leo--translate-word-click-search-more-pos (_event)
  "Translate word on mouse click EVENT between `leo-language' and German."
  (interactive "e")
  (let ((lang (or (plist-get leo--results-info 'lang) ;stored prefix lang choice
                  leo-language)) ;fallback
        (text (plist-get leo--results-info 'term)))
    (leo--translate-more-pos lang
                             text
                             (thing-at-point 'sentence t)))) ; POS heading itself

(defun leo--translate-word-return-search-more-pos ()
  "Translate word or phrase at point between `leo-language' and German.
Word or phrase at point is determined by button text property."
  (interactive)
  (let ((lang (or (plist-get leo--results-info 'lang) ;stored prefix lang choice
                  leo-language)) ;fallback
        (text (plist-get leo--results-info 'term)))
    (leo--translate-more-pos lang
                             text
                             (thing-at-point 'sentence t)))) ; POS heading itself

(defun leo-previous-heading ()
  "Move point to previous POS or forum heading."
  (interactive)
  (save-match-data
    (let ((match
           (save-excursion
             (text-property-search-backward 'heading ;NB 27.1!
                                            t t t))))
      (if match
          (progn
            (goto-char (prop-match-beginning match))
            (recenter-top-bottom 3))
        (message "No more headings.")))))

(defun leo-next-heading ()
  "Move point to next POS or forum heading."
  (interactive)
  (save-match-data
    (let ((match
           (save-excursion
             (text-property-search-forward 'heading ;NB 27.1!
                                           t t t))))
      (if match
          (progn
            (goto-char (prop-match-beginning match))
            (recenter-top-bottom 3))
        (message "No more headings.")))))

(defun leo-jump-to-forum-results ()
  "Jump to forum results."
  (interactive)
  (goto-char (point-max))
  (leo-previous-heading))

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
      "\n\nHit 't'/'s' to search again.\n\n"))))

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
  (let ((inhibit-read-only t)
        (word-spl (split-string word)))
    (save-excursion
      (goto-char (point-min))
      (mapc (lambda (x)
              (while (search-forward-regexp (concat "\\b" x "\\b")
                                            nil 'noerror)
                (add-text-properties (- (point) (length x)) (point)
                                     '(face leo-match-face)))
              (goto-char (point-min)))
            word-spl))))

(defun leo--print-results-buffer-heading (word)
  "Insert heading in buffer showing results for WORD."
  (with-current-buffer (get-buffer " *leo*")
    (insert
     (propertize
      (concat "leo.de search results for " word ":")
      'face 'leo-search-and-forum-face
      'heading t)
     "\n\n")))

(defun leo--print-results-buffer-forum-heading (word)
  "Insert forum results heading in buffer showing results for WORD."
  (with-current-buffer (get-buffer " *leo*")
    (insert
     (propertize
      (concat "leo.de forum results for " word ":\n\n")
      'face 'leo-search-and-forum-face
      'heading t))))

(defun leo--open-translation-buffer (results forums word lang similar)
  "Print translation RESULTS and FORUMS in temporary buffer.
The search term WORD is propertized in results. The search is
between LANG and German. SIMILAR is a list of suggestions to
display if there are no results."
  (with-output-to-temp-buffer " *leo*" ; this makes it help-mode
    (leo--print-results-buffer-heading word)
    (leo--print-translation results word similar)
    (leo--print-results-buffer-forum-heading word)
    (leo--print-forums forums))
  ;; make rly sure we are in correct buffer
  ;; before we do anything further
  (with-current-buffer (get-buffer " *leo*")
    (leo--make-buttons)
    (leo--propertize-search-term-in-results word)
    (leo-mode)
    (setq leo--results-info `(term ,word lang ,lang)))
  (if (not (equal (buffer-name (current-buffer)) " *leo*"))
      (switch-to-buffer-other-window (get-buffer " *leo*")))
  ;; (other-window 1))
  (message (concat "'t'/'s': search again, prefix: set language,\
 '.'/',': next/prev heading, 'f': jump to forums, 'b': view in browser,\
 '<'/'>': search in left/right lang only, 'l': search on linguee.de"
                   (when (require 'helm-dictionary nil :noerror)
                     ", 'd': search in helm-dictionary")
                   (when (require 'dictcc nil :noerror)
                     ", 'c': search with dictcc.el"))))

(defun leo-translate-single-side (side)
  "Retranslate last term searching only in SIDE.
SIDE is a string of either \"left\" or \"right\"."
  (let ((word (plist-get leo--results-info 'term))
        (lang (plist-get leo--results-info 'lang)))
    (leo--translate lang word nil side)))

(defun leo-translate-left-side-only ()
  "Retranslate last term searching only on the left side."
  (interactive)
  (leo-translate-single-side "left"))

(defun leo-translate-right-side-only ()
  "Retranslate last term searching only on the right side."
  (interactive)
  (leo-translate-single-side "right"))

(defun leo--translate (lang word &optional pos side)
  "Translate WORD between LANG and German.
SIDE is the side to search in, either \"left\" or \"right\"."
  (let* ((xml (leo--parse-xml
               (leo--generate-url lang word pos side)))
         (section-list (car (leo--get-result-section-list (car xml))))
         ;; similar terms to offer if no results:
         (similar-list (car (leo--get-result-similar-list (car xml)))))
    (leo--open-translation-buffer
     (leo--build-sections-list section-list)
     (leo--extract-forum-subject-link-pairs xml)
     word
     lang
     similar-list)))

(defun leo--translate-more-pos (lang word pos)
  "Translate WORD between LANG and German.
Return 30 results for a single POS, rather than 16 for every POS."
  (leo--translate lang word pos))

;;;###autoload
(defun leo-translate-word (&optional prefix)
  "Translate a word between language set by `leo-language' and German.
Show translations in new buffer window. Term to translate is
either the current region, word at point, or input by the user.
Optional arg PREFIX prompts to set language for this search."
  (interactive "P")
  (let* ((language-candidates
          ;; transpose alist for comp read to display full lang name
          (mapcar (lambda (x)
                    (cons (cdr x) (car x)))
                  leo-languages-full))
         ;; get stored lang if we are already in a results page:
         (lang-stored (or (plist-get leo--results-info 'lang) ;stored prefix lang choice
                          leo-language)) ;fallback
         (region (if (and (equal major-mode 'pdf-view-mode)
                          (region-active-p))
                     (car (pdf-view-active-region-text))
                   (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
         (word
          (read-string (format "Translate (%s): " (or region (current-word) ""))
                       nil nil (or region (current-word)))))
    (if prefix
        ;; if prefix: prompt for language to search for:
        (let ((lang-prefix (completing-read
                            (format "Language (to pair with German) (%s): "
                                    (car (rassoc leo-language language-candidates)))
                            language-candidates nil t nil nil
                            (rassoc leo-language language-candidates))))
          (leo--translate (cdr (assoc lang-prefix language-candidates))
                          word))
      ;; else normal search:
      (leo--translate lang-stored word))))

(define-derived-mode leo-mode special-mode "leo"
  :group 'leo
  (read-only-mode 1))

(provide 'leo)
;;; leo.el ends here
