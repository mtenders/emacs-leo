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
  "Returns a list of term plus any phrasal suffixes or variants from an side's WORDS."
  (let* ((term (car words))
         (suffix-list
          (mapcar (lambda (x)
                    (replace-regexp-in-string (concat term " ") "" x))
                  words)))
    (list (cons 'term term)
          (cons 'suffixes (delete term (cdr suffix-list))))))

(defun leo--extract-words-from-side (side)
  (leo--extract-phrases-from-words-list
   (leo--extract-word-strings-as-list
    (leo--get-words-node-from-side side))))

;; ONLY FOR VERBS
(defun leo--extract-cases-from-side (side)
  "Extract a term's case from a given SIDE.
A side is either the source or target result for a given search."
  (let* ((repr (xml-get-children side 'repr))
         (smalls (leo--map-get-children repr 'small))
         (sups (leo--map-get-children smalls 'sup))
         (ms (leo--map-get-children sups 'm))
         (cases (leo--map-get-children ms 't)))
    (mapcar (lambda (x)
              (leo--strip-trailing-period
               (caddr x)))
            cases)))

;; for PREPS, ADJ, ADV:
(defun leo--extract-tags-cases-and-pos-from-side (side)
  "Extract a term's info tags, case and part of speech from a given SIDE.
This runs for prepositions, adjectives and adverbs.
Orders POS to come first, so that info tags can run on with what follows"
  (let* ((repr (xml-get-children side 'repr))
         (smalls (leo--map-get-children repr 'small))
         (is (leo--map-get-children smalls 'i))
         (ms (leo--map-get-children is 'm))
         (ts (leo--map-get-children ms 't))
         (cases-and-pos (mapcar (lambda (x)
                                  (leo--strip-trailing-period
                                   (caddr x)))
                                ts)))
    ;; awful hack to display POS before anything else:
    (if (> (length cases-and-pos) 1)
        (let* ((pos '("adv" "adj" "Präp"))
               (intersect (seq-intersection cases-and-pos pos)))
          (if intersect
              (car
                    (mapcar (lambda (x)
                              (let ((culled (delete x cases-and-pos)))
                                (cons x culled)))
                            intersect))))
      cases-and-pos)))

;; for NOUNS only??
(defun leo--extract-tags-from-side (side)
  "Extract a term's info tags from a given SIDE.
Tags include register (eg 'coll' or 'fig') and helper markers like 'also.', 'or:', for prefixing other elements."
  (let* ((repr (xml-get-children side 'repr))
         (smalls (leo--map-get-children repr 'small))
         (is (leo--map-get-children smalls 'i))
         (ms (leo--map-get-children is 'm))
         (ts (leo--map-get-children ms 't)))
    (mapcar (lambda (x)
              (leo--strip-trailing-period
               (caddr x)))
            ts)))

;; for NOUNS only | works for DE at least
(defun leo--extract-scientific-name-from-side (side)
  "Extract a term's info tags from a given SIDE.
Tags include register (eg 'coll' or 'fig') and helper markers like 'also.', 'or:', for prefixing other elements."
  (let* ((repr (xml-get-children side 'repr))
         (smalls (leo--map-get-children repr 'small))
         (is (leo--map-get-children smalls 'i))
         (ms (leo--map-get-children is 'm))
         (ts (leo--map-get-children ms 't)))
    (if (member "wiss.:"
                (leo--extract-tags-from-side side))
        ;; TODO: test for wiss. in ts children
        (remove nil
                (mapcar (lambda (x)
                          (let ((sci-mayb (car (last (xml-node-children x)))))
                            (when (stringp sci-mayb)
                              (if (string-match "^[ ]+" sci-mayb)
                                  (substring sci-mayb 1 nil)))))
                        is)))))

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

(defun leo--extract-domains-from-side (side)
  "Extract a term's domains from a given SIDE.
A side is either the source or target result for a given search."
  (let* ((repr (xml-get-children side 'repr))
         (lang (leo--get-lang-from-side side)))
    (cond ((equal lang "de")
           (let* ((virr (leo--get-child repr 'virr))
                  (small (leo--get-child virr 'small))
                  (i (leo--get-child small 'i))
                  (m (leo--get-child i 'm))
                  (tag (leo--get-child m 't)))
             (caddr tag)))
          (t ; en and fr work like this at least
           (let* ((domains (leo--map-get-children repr 'domain))
                  (smalls (leo--map-get-children domains 'small))
                  (ms (leo--map-get-children smalls 'm))
                  (tags (leo--map-get-children ms 't)))
             (if tags
                 (mapcar (lambda (x)
                           (leo--strip-trailing-period (caddr x)))
                         tags)))))))

(defun leo--extract-plural-from-side (side)
  "Extract a term's plural form from a given SIDE.
A side is either the source or target result for a given search.
Returns a string ."
  (let* ((repr (leo--get-child side 'repr))
         (lang (leo--get-lang-from-side side)))
    (cond ((equal lang "de")
           (let* ((flecttabref (leo--get-child repr 'flecttabref))
                  (small (leo--get-child flecttabref 'small)))
             (cadddr small)))
          (t ; equal lang "en")
           (let ((small (leo--get-child repr 'small)))
             (cadddr small))))))

;; EN nouns have these, at the least
(defun leo--extract-context-marker-from-side (side)
  "Extract a term's context marker from a given SIDE.
A context marker disambiguates what particular meaning of a term is being used for a result."
  (let* ((repr (xml-get-children side 'repr))
         (lang (leo--get-lang-from-side side)))
    (cond ;((equal lang "de")
           ;;FIXME: add this flecttabref to the list created for cond t:
           ;; (let* ((flecttabref (leo--get-child repr 'flecttabref))
                  ;; (small (leo--get-child flecttabref 'small)))
             ;; (cadddr small)))
          (t ;; (if (equal lang "en")
           (let* ((smalls (leo--map-get-children repr 'small))
                  (is (leo--map-get-children smalls 'i)))
             (remove nil ; cull nils from our mapping
                     (mapcar (lambda (x)
                               (if (stringp (caddr x))
                                   ;;in case we get a bunch more XML instead
                                   (if (> (length (caddr x)) 1)
                                       ;; in case we get a lone ] as part of a tag
                                       (leo--strip-redundant-parens
                                        (caddr x)))))
                             is)))))))


(defun leo--extract-abbrev-from-side (side)
  "Extract a term's abbreviated form from a given SIDE."
  (let* ((search (leo--get-child side 'search))
         (search-words (xml-get-children search 'word))
         ;; HACK: abbrev appears as last of the words in search node
         (last-word (car (last search-words))))
    (if (or (member "Abk.:" (leo--extract-tags-from-side side))
            (member "abbr.:" (leo--extract-tags-from-side side)))
        (car (xml-node-children last-word)))))

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
  (let ((pos (leo--get-entry-part-of-speech entry))
        (sides (leo--get-sides-from-entry entry)))
    (mapcar (lambda (x)
              (cond ((string= pos "noun")
                     (list (leo--extract-words-from-side x)
                           (cons 'pl (leo--extract-plural-from-side x))
                           (cons 'domains (leo--extract-domains-from-side x))
                           (cons 'tags (leo--extract-tags-from-side x))
                           (cons 'sci (leo--extract-scientific-name-from-side x))
                           (cons 'abbrev (leo--extract-abbrev-from-side x))
                           (cons 'context (leo--extract-context-marker-from-side x))
                           (cons 'table (leo--extract-flextable-from-side x))))
                    ((string= pos "verb")
                     (list (leo--extract-words-from-side x)
                           ;; (cons 'pl (leo--extract-plural-from-side x))
                           (cons 'domains (leo--extract-domains-from-side x))
                           (cons 'abbrev (leo--extract-abbrev-from-side x))
                           (cons 'cases (leo--extract-cases-from-side x))
                           ;; (cons 'context (leo--extract-context-marker-from-side x))
                           (cons 'table (leo--extract-flextable-from-side x))))
                    ((if (or (string= pos "adjective")
                             (string= pos     "adverb"))
                         (list (leo--extract-words-from-side x)
                               ;; (cons 'pl (leo--extract-plural-from-side x))
                               (cons 'domains (leo--extract-domains-from-side x))
                               (cons 'cases (leo--extract-tags-cases-and-pos-from-side x))
                               (cons 'abbrev (leo--extract-abbrev-from-side x))
                               (cons 'context (leo--extract-context-marker-from-side x))
                               (cons 'table (leo--extract-flextable-from-side x)))))
                    ((string= pos "preposition")
                     (list (leo--extract-words-from-side x)
                           (cons 'domains (leo--extract-domains-from-side x))
                           (cons 'cases (leo--extract-tags-cases-and-pos-from-side x))
                           (cons 'abbrev (leo--extract-abbrev-from-side x))
                           ;; (cons 'context (leo--extract-context-marker-from-side x))
                           (cons 'table (leo--extract-flextable-from-side x))))
                    ;; FIXME Add other entry types
                    ;; interjection,
                    (t ; a generic entry
                     (list (leo--extract-words-from-side x)
                           (cons 'pl (leo--extract-plural-from-side x))
                           (cons 'domains (leo--extract-domains-from-side x))
                           (cons 'cases (leo--extract-cases-from-side x))
                           (cons 'abbrev (leo--extract-abbrev-from-side x))
                           ;; (cons 'context (leo--extract-context-marker-from-side x))
                           (cons 'table (leo--extract-flextable-from-side x))))))
            sides)))



;;; PRINTING

(defun leo--print-single-side (side)
  (let* ((term (cdr (assoc 'term (car side))))
         (suffixes (cdr (assoc 'suffixes (car side))))
         (plural-full (cdr (assoc 'pl (cdr side))))
         (plural (if (and (stringp plural-full)
                          (string-match "^[ ]+" plural-full))
                     (substring plural-full 1 nil)))
         (tags (cdr (assoc 'tags (cdr side))))
         (domains (cdr (assoc 'domains (cdr side))))
         (case-marks (cdr (assoc 'cases (cdr side))))
         (sci (cdr (assoc 'sci (cdr side))))
         (abbrev (cdr (assoc 'abbrev (cdr side))))
         (context (cdr (assoc 'context (cdr side))))
         (table (cdr (assoc 'table (cdr side)))))
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
            'keymap leo-inflexion-table-map ;shr-map
            'fontified t
            'face 'leo--auxiliary-face
            'mouse-face 'highlight
            'help-echo (concat "Browse inflexion table for '"
                               term "'"))
           " "))
      (propertize term
                  'button t
                  'follow-link t
                  'keymap leo-result-search-map
                  'fontified t
                  'face 'leo--link-face
                  'mouse-face 'highlight
                  'help-echo (concat "Click to search leo for this term"))
      (if case-marks
          (propertize (concat " ("
                              (mapconcat #'identity case-marks ", ")
                              ")")
                      'face 'leo--auxiliary-face))
      (if tags
          (propertize (concat " ("
                              (mapconcat #'identity tags ", ")
                              ")")
                      'face '(slanted italic inherit font-lock-comment-face))) ;'leo--auxiliary-face))
      (if suffixes
          (propertize (concat " "
                              (mapconcat #'identity suffixes ", "))
                      'face 'leo--auxiliary-face))
      (if sci
          (propertize (concat " "
                              (mapconcat #'identity sci ", "))
                      'face '(slanted italic inherit font-lock-comment-face)))
      (if abbrev
          (propertize (concat " " abbrev)
                      'face 'leo--auxiliary-face))
      (if (and plural (stringp plural))
          (concat " "
                  (propertize plural
                              ;; 'button t ; no redundant tab stops
                              'follow-link t
                              'shr-url table
                              'keymap leo-inflexion-table-map ;shr-map
                              'fontified t
                              'face 'leo--auxiliary-face
                              'mouse-face 'highlight
                              'help-echo (concat "Browse inflexion table for '"
                                                 term "'"))))
      (if domains
          (propertize (concat " ["
                              (mapconcat #'identity domains ",")
                              "]")
                      'face 'leo--auxiliary-face))
      (if context
          (propertize (concat " {"
                              (mapconcat #'identity context ", ")
                              "}")
                      'face 'leo--auxiliary-face))))))

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

(defun leo--print-translation (results word similar)
  "Format and print translation RESULTS."
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
                        'face 'leo--link-face
                        'mouse-face 'highlight
                        'help-echo (concat "Browse forum entry for '"
                                           (caar forum-posts) "'")))
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

(defun leo-browse-url-results ()
  "Open the current results for LANG and WORD in external browser."
  (interactive)
  (let ((lang-full (cdr (assoc leo-language leo-languages-full)))
        (word (plist-get leo--results-info 'term)))
    (browse-url-xdg-open
     (concat "https://dict.leo.org/" lang-full "-deutsch/" word))))

(defun leo--translate-word-click-search (event)
  "Translate word on mouse click EVENT from language set by 'leo-language' to German."
  (interactive "e")
  (leo--translate leo-language (word-at-point)))

(defun leo--translate-word-return-search ()
  "Translate word on hitting return from language set by 'leo-language' to German."
  (interactive)
  (let ((text (buffer-substring-no-properties (point) ; we already tabbed to here
                                              (next-single-property-change
                                               (point) 'button))))
    (leo--translate leo-language text)))

(defun leo--did-you-mean (word similar)
  "Print some alternative terms to search for.
Used if `leo--print-translation' has no results. Results are links to searches for themselves."
  (let* ((sim-sides (xml-get-children similar 'side))
         (sim-word-nodes (leo--map-get-children sim-sides 'word))
         (sim-word-strings
          (mapcar (lambda (x)
                    (car (xml-node-children x)))
                  sim-word-nodes))
         (sim-words-propertized
          (mapcar (lambda (x)
                    ;; (let ((sim-map (make-sparse-keymap)))
                      ;; (define-key sim-map [mouse-2] 'leo--translate-word-click-search)
                      (propertize x
                                  'button t
                                  'follow-link t
                                  'keymap leo-result-search-map
                                  'fontified t
                                  'face 'leo--link-face
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
        (let ((props (text-properties-at (- (point) 1))))
          (remove-text-properties (- (point) (length word)) (point)
                                  '(face face))
          (add-text-properties (- (point) (length word)) (point)
                               '(face leo--match-face)))))))

(defun leo--print-results-buffer-heading (word)
  (with-current-buffer (get-buffer " *leo*")
    (insert
     (propertize
      (concat "leo.de search results for " word ":\n\n")
      'face 'leo--search-and-forum-face))))

(defun leo--print-results-buffer-forum-heading (word)
    (with-current-buffer (get-buffer " *leo*")
      (insert
       (propertize
        (concat "leo.de forum results for " word ":\n\n")
        'face 'leo--search-and-forum-face))))

(defun leo--open-translation-buffer (results forums word similar)
  "Print translation RESULTS and FORUMS in temporary buffer.
The search term WORD is propertized in results."
  (with-output-to-temp-buffer " *leo*" ; temp-buffer-show-hook makes it help-mode
    (leo--print-results-buffer-heading word)
    (leo--print-translation results word similar)
    (leo--print-results-buffer-forum-heading word)
    (leo--print-forums forums))
  (if (not (equal (buffer-name (current-buffer)) " *leo*"))
      (other-window 1))
  (leo--make-buttons)
  (leo--propertize-search-term-in-results word)
  ;; hack to not ruin help-mode bindings, till we have a minor mode:
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "t") 'leo-translate-word)
  (local-set-key (kbd "b") 'leo-browse-url-results)
  (when (require 'dictcc nil :noerror)
    (local-set-key (kbd "c") 'dictcc))
  (setq leo--results-info `(term ,word)))

(defun leo--translate (lang word)
  "Translate WORD from LANG to German."
  (let* ((xml (leo--parse-xml
              (leo--generate-url lang word)))
         (section-list (car (leo--get-result-section-list (car xml))))
         (similar-list (car (leo--get-result-similar-list (car xml)))))
    (leo--open-translation-buffer
     (leo--build-sections-list section-list)
     (leo--extract-forum-subject-link-pairs xml)
     word
     similar-list)))

;;;###autoload
(defun leo-translate-word (word)
  "Translate WORD from language set by 'leo-language' to German.
Show translations in new buffer window."
  (interactive "sTranslate: ")
  (leo--translate leo-language word)
  (message "Hit 't' to search again."))

;;;###autoload
(defun leo-translate-at-point ()
  "Translate word under cursor from language set by 'leo-language' to German.
Show translations in new buffer windown."
  (interactive)
  (leo--translate leo-language (current-word)))


(provide 'leo)
;;; leo.el ends here
