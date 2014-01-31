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
     (:background "beige" :foreground "black"))
    (((class color) (background dark))
     (:background "purple" :foreground "white")))
  "Face for weights information higlights.")

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
  (when org-weights-mode
    (save-excursion
      (goto-char (point-min))
      (outline-next-visible-heading 1)
      (while (not (eobp))
        (save-excursion
          (org-weights-set-overlay (org-weights-at-point)))
        (outline-next-visible-heading 1))
      (add-hook 'after-change-functions 'org-weights-after-change
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
                (org-weights-set-overlay (org-weights-at-point)))
              (save-excursion
                (while (outline-up-heading 1)
                  (org-weights-set-overlay (org-weights-at-point))))
              (setq force nil))
          (error nil))))))

;;;; Routines

(defun org-weights-set-overlay (weights)
  "Put an overlays on the current line, displaying WEIGHTS."
  (let ((headers (car weights))
        (paragraphs (cdr weights))
        overlay)
    (beginning-of-line)
    (skip-chars-forward "*")
    (let ((overlays org-weights-overlays))
      (while overlays
        (let ((candidate (pop overlays)))
          (when (and (>= (point) (overlay-start candidate))
                     (<= (point) (overlay-end candidate)))
            (setq overlay candidate
                  overlays nil)))))
    (unless overlay
      (setq overlay (make-overlay (1- (point)) (point) nil t)))
    (let ((text (concat
                 (buffer-substring (1- (point)) (point))
                 (org-add-props
                     (if (zerop headers)
                         (format " %3s    " paragraphs)
                       (format " %3s %2s " paragraphs headers))
                     (list 'face 'org-weights-face)))))
      (if (not (featurep 'xemacs))
          (overlay-put overlay 'display text)
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'end-glyph (make-glyph text)))
      (push overlay org-weights-overlays))))

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
