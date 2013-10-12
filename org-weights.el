;;; org-weights.el --- Show how heavy Org subtrees are.

;; Copyright © 2013 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;; Maintainer: François Pinard <pinard@iro.umontreal.ca>
;; URL: https://github.com/pinard/org-weights

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; Display the weights for every Org header which was visible at the
;; time the mode was activated.  The weights of a header are the
;; counts of subtrees and paragraphs for the the subtree starting with
;; that header.  Paragraphs includes items and other equivalent
;; structures.

;;; Code:

(defface org-weights-face
  '((((class color) (background light))
     (:foreground "chocolate2" :weight bold))
    (((class color) (background dark))
     (:foreground "chocolate1" :weight bold)))
  "Face for weights information higlights.")

(defconst org-weights-overlay-column 75
  ;; Should be high enough for tags to stay visible.
  "Column where the weight overlay should be displayed.")

(defvar org-weights-overlays nil
  "Running list of currently displayed overlays.")
(make-variable-buffer-local 'org-weights-overlays)

(defvar org-weights-saved-start nil
  "Header start position if, before command, point was within a header line.")

(define-minor-mode org-weights-mode
  "Show header weights in the entire buffer."
  nil nil nil
  (mapc 'delete-overlay org-weights-overlays)
  (setq org-weights-overlays nil)
  (remove-hook 'after-change-functions
               'org-weights-after-change 'local)
  (remove-hook 'pre-command-hook
               'org-weights-pre-command 'local)
  (remove-hook 'post-command-hook
               'org-weights-post-command 'local)
  (when org-weights-mode
    (save-excursion
      (goto-char (point-min))
      (outline-next-visible-heading 1)
      (while (not (eobp))
        (save-excursion
          (org-weights-set-overlay (org-weights-at-point)
                                     (funcall outline-level)))
        (outline-next-visible-heading 1))
      (add-hook 'after-change-functions 'org-weights-after-change
                nil 'local)
      (add-hook 'pre-command-hook 'org-weights-pre-command
                nil 'local)
      (add-hook 'post-command-hook 'org-weights-post-command
                nil 'local))))

;;;; Hooks.

(defun org-weights-after-change (begin end replaced)
  "Recompute overlays for all headers between BEGIN and END, and up for each."
  (save-match-data
    (save-excursion
      (let ((bol (point-at-bol))
            (force t))
        (goto-char end)
        (condition-case nil
            (while (and (outline-back-to-heading)
                        (or force (>= (point) begin)))
              (unless (= (point) bol)
                (org-weights-set-overlay (org-weights-at-point)
                                           (funcall outline-level)))
              (save-excursion
                (while (outline-up-heading 1)
                  (org-weights-set-overlay (org-weights-at-point)
                                             (funcall outline-level))))
              (setq force nil))
          (error nil))))))

(defun org-weights-pre-command ()
  "Save point if it stands within a header line."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (setq org-weights-saved-start
            (and (outline-on-heading-p)
                 (point-marker))))))

(defun org-weights-post-command ()
  "If point went on another line, remove the overlay when a header line.
Also, if the line changed, recompute the overlay for saved point."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (unless (and org-weights-saved-start
                   (= (point) org-weights-saved-start))
        (when (outline-on-heading-p)
          (org-weights-unset-overlay))
        (when org-weights-saved-start
          (let ((buffer (marker-buffer org-weights-saved-start)))
            (when buffer
              (set-buffer buffer)
              (goto-char org-weights-saved-start)
              (org-weights-set-overlay (org-weights-at-point)
                                         (funcall outline-level)))))))))

;;;; Routines

(defun org-weights-set-overlay (weights level)
  "Put an overlays on the current line, displaying WEIGHTS.
Prefix weights with LEVEL number of stars."
  (let ((level-string
         (make-string (if level (org-get-valid-level level 0) 0) ?*))
        (headers (car weights))
        (paragraphs (cdr weights))
        filler overlay)
    (org-move-to-column org-weights-overlay-column)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (let ((overlays org-weights-overlays))
      (while overlays
        (let ((maybe (pop overlays)))
          (if (and (>= (point) (overlay-start maybe))
                   (<= (point) (overlay-end maybe)))
              (setq overlay maybe
                    overlays nil)))))
    (unless overlay
      (setq overlay (make-overlay (1- (point)) (point-at-eol))))
    (setq filler (max 0 (- org-weights-overlay-column
                           (current-column) 2)))
    (let ((text (concat
                 (buffer-substring (1- (point)) (point))
                 (make-string (+ 2 filler) ? )
                 (org-add-props
                     (format "%s %3s%s" level-string paragraphs
                             (if (zerop headers) ""
                               (format " + %s" headers)))
                     (list 'face 'org-weights-face)))))
      (if (not (featurep 'xemacs))
          (overlay-put overlay 'display text)
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'end-glyph (make-glyph text)))
      (push overlay org-weights-overlays))))

(defun org-weights-unset-overlay ()
  "Remove an overlay from the current line, so it gets edited more easily."
  (let (overlay)
    (org-move-to-column org-weights-overlay-column)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (let ((overlays org-weights-overlays))
      (while (and overlays (not overlay))
        (let ((maybe (pop overlays)))
          (if (and (>= (point) (overlay-start maybe))
                   (<= (point) (overlay-end maybe)))
              (setq overlay maybe)))))
    (when overlay
      (setq org-weights-overlays (delq overlay org-weights-overlays))
      (delete-overlay overlay))))

;; Compliment of Nicolas Goaziou <n.goaziou@gmail.com>, 2012-02-26
(defun org-weights-at-point ()
  "Return cons of number of subtrees and paragraphs in the subtree at point.
Paragraphs (also encompasses equivalent structures)."
  (org-with-wide-buffer
   (org-narrow-to-subtree)
   (let ((tree (org-element-parse-buffer 'element)) (num-hl 0) (num-el 0))
     (org-element-map tree 'headline (lambda (hl) (incf num-hl)))
     (org-element-map
      tree '(paragraph table verse-block quote-block src-block example-block)
      (lambda (el) (incf num-el)))
     (cons (1- num-hl) num-el))))

;;; org-weights.el ends here
