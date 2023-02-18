;;; org-inline-clocking-buttons.el --- Buttons inline next to org headings for clocking in and out   -*- lexical-binding:t -*-

;; Copyright (C) 2022 ParetoOptimalDev.

;; Author: ParetoOptimalDev <pareto.optimal@mailfence.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: outlines, hypermedia, clocking, calendar
;; URL: https://github.com/ParetoOptimalDev/org-inline-clocking-buttons
;;; Commentary:

;; This package provides a minor mode to add buttons to an org heading inline  for clocking in and out

;;; Code:
(defface org-inline-clock-button-face
  '((t (:inherit custom-button)))
  "Face for GFM checkboxes.")

(define-button-type 'org-clock-in-button
  'follow-link t
  'face 'org-inline-clock-button-face
  'mouse-face 'custom-button-mouse
  'action (lambda (b) (org-clock-in)))

(define-button-type 'org-clock-out-button
  'follow-link t
  'face 'org-inline-clock-button-face
  'mouse-face 'custom-button-mouse
  'action (lambda (b) (org-clock-out)))

(defun has-clock-button (str)
  (let ((heading-string (nth 4 (org-heading-components))))
    (and heading-string (s-contains? str heading-string))))

(defun has-clock-out-button ()
  (has-clock-button "Clock Out"))

(defun has-clock-in-button ()
  (has-clock-button "Clock In"))

(defun remove-clock-buttons-current-line ()
  (let ((line-start-pos (progn (beginning-of-line) (point)))
	(line-end-pos (progn (end-of-line) (point))))
    (save-excursion
      (save-restriction
	(widen)
	(remove-overlays line-start-pos line-end-pos 'face 'custom-button)
	(replace-string " Clock In" "" nil line-start-pos line-end-pos)
	(replace-string "    Clock Out" "" nil line-start-pos line-end-pos)))))

(defun pod/add-clock-in-button-to-right-of-heading ()
  (unless (org-at-heading-p) (org-previous-visible-heading 1))
  (unless (has-clock-in-button)
    (remove-clock-buttons-current-line)
    (save-excursion
      (org-end-of-line)
      (let ((end-of-line-before-insert (point)))
	(insert "    Clock In")
	(let* ((button-start (+ 4 end-of-line-before-insert))
	       (button-end (+ 8 button-start)))
	  (make-button button-start button-end :type 'org-clock-in-button))))))

(defun pod/add-clock-out-button-to-right-of-heading ()
  (unless (org-at-heading-p) (org-previous-visible-heading 1))
  (unless (has-clock-out-button)
    (remove-clock-buttons-current-line)
    (save-excursion
      (org-end-of-line)
      (let ((end-of-line-before-insert (point)))
	(insert "    Clock Out")
	(let* ((button-start (+ 4 end-of-line-before-insert))
	       (button-end (+ 9 button-start)))
	  (make-button button-start button-end :type 'org-clock-out-button))))))

(defun pod/remove-org-inline-clock-button-overlays ()
  "Remove all org inline clock butotn overlays in buffer."
  (save-excursion
    (save-restriction
      (widen)
      (remove-overlays nil nil 'face 'org-inline-clock-button-face)
      ;; TODO improve this by making incorrect replacement less likely
      ;; maybe make text behind buttons more unique
      (replace-regexp "    Clock In" "" nil (point-min) (point-max))
      (replace-regexp "       Clock Out" "" nil (point-min) (point-max)))))

;;;###autoload
(define-minor-mode org-inline-clocking-buttons-mode
  "org inline clocking buttons mode"
  (if org-inline-clocking-buttons-mode
      (add-hook 'org-after-todo-state-change-hook #'pod/add-clock-in-button-to-right-of-heading)
    (add-hook 'org-clock-in-hook #'pod/add-clock-out-button-to-right-of-heading)
    (add-hook 'org-clock-out-hook #'pod/add-clock-in-button-to-right-of-heading))
  (pod/remove-org-inline-clock-button-overlays))
