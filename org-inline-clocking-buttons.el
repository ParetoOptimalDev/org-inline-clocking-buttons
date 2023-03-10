;;; org-inline-clocking-buttons.el --- Buttons inline next to org headings for clocking in and out   -*- lexical-binding:t -*-

;; Copyright (C) 2022 ParetoOptimalDev.

;; Author: ParetoOptimalDev <pareto.optimal@mailfence.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (svg-lib "0.2.5"))
;; Keywords: outlines, hypermedia, clocking, calendar
;; URL: https://github.com/ParetoOptimalDev/org-inline-clocking-buttons
;;; Commentary:

;; This package provides a minor mode to add buttons to an org heading inline  for clocking in and out

;;; Code:
(require 'org)
(defface org-inline-clocking-buttons-clock-button-face
  '((t (:inherit custom-button)))
  "Face for GFM checkboxes.")

(define-button-type 'org-clock-in-button
  'follow-link t
  'face 'org-inline-clocking-buttons-clock-button-face
  ;; 'mouse-face 'custom-button-mouse
  'action (lambda (_) (org-clock-in)))

(define-button-type 'org-clock-out-button
  'follow-link t
  'face 'org-inline-clocking-buttons-clock-button-face
  ;; 'mouse-face 'custom-button-mouse
  'action (lambda (_) (org-clock-out)))

(defun org-inline-clocking-buttons-has-clock-button (str)
  "Check if the clock button string STR is in the current org heading."
  (let ((heading-string (nth 4 (org-heading-components))))
    (and heading-string  (string-match-p (regexp-quote str) heading-string))))

(defun org-inline-clocking-buttons-has-clock-out-button ()
  "Check if clock out button is on current org heading."
  (org-inline-clocking-buttons-has-clock-button " "))

(defun org-inline-clocking-buttons-has-clock-in-button ()
  "Check if clock in button is on current org heading."
  (org-inline-clocking-buttons-has-clock-button " "))

(defun org-inline-clocking-buttons-remove-clock-buttons-current-line ()
  "Remove all clock buttons on the current line."
  (let ((line-start-pos (progn (beginning-of-line) (point)))
	(line-end-pos (progn (end-of-line) (point))))
    (save-excursion
      (save-restriction
	(widen)
	(remove-overlays line-start-pos line-end-pos 'face 'org-inline-clocking-buttons-clock-button-face)
	(progn
	  (beginning-of-line)
	  (when (search-forward "      " line-end-pos t)
	    (replace-match "" nil t)))
	(progn
	  (beginning-of-line)
	  (when (search-forward "     " line-end-pos t)
	    (replace-match "" nil t)))))))

(defun org-inline-clocking-buttons-add-clock-in-button-to-right-of-heading ()
  "Add a `Clock In` button to the right of the current org heading."
  (unless (org-at-heading-p) (org-previous-visible-heading 1))
  (unless (org-inline-clocking-buttons-has-clock-in-button)
    (org-inline-clocking-buttons-remove-clock-buttons-current-line)
    (save-excursion
      (org-end-of-line)
      (let ((end-of-line-before-insert (point)))

	(progn
	  (insert "    ")
	  (insert-image
	   (svg-lib-icon "play-circle" nil
			 :collection "material"
			 :stroke 0
			 :scale 1
			 :padding 0)))
	(let* ((button-start (+ 3 end-of-line-before-insert))
	       (button-end (+ 2 button-start)))
	  (make-button button-start button-end
		       :type 'org-clock-in-button))))))

(defun org-inline-add-play-button-to-todo-headings ()
  (org-map-entries #'org-inline-add-play-button-and-svg "/TODO???=\"TODO\""))

(defun org-inline-add-play-button-and-svg ()
  (end-of-line)
  (insert "    ")
  (let* ((button-start (- (point) 1))
	(button-end (+ 2 button-start)))
  (insert-image
   (svg-lib-icon "play-circle" nil
		 :collection "material"
		 :stroke 0
		 :scale 1
		 :padding 0))
  (make-button button-start button-end
		       :type 'org-clock-in-button)))

(defun org-inline-clocking-buttons-add-clock-out-button-to-right-of-heading ()
  "Add a `Clock Out` button to the right of the current org heading."
  (unless (org-at-heading-p) (org-previous-visible-heading 1))
  (unless (org-inline-clocking-buttons-has-clock-out-button)
    (org-inline-clocking-buttons-remove-clock-buttons-current-line)
    (save-excursion
      (org-end-of-line)
      (let ((end-of-line-before-insert (point)))
	(progn
	  (insert "    ")
	  (insert-image (svg-lib-icon "stop-circle" nil
				      :collection "material"
				      :stroke 0
				      :scale 1
				      :padding 0)))
	(let* ((button-start (+ 3 end-of-line-before-insert))
	       (button-end (+ 2 button-start)))
	  (make-button button-start button-end :type 'org-clock-out-button))))))

(defun org-inline-clocking-buttons-remove-org-inline-clock-button-overlays ()
  "Remove all org inline clock butotn overlays in buffer."
  (save-excursion
    (save-restriction
      (widen)
      (remove-overlays nil nil 'face 'org-inline-clocking-buttons-clock-button-face)
      ;; TODO improve this by making incorrect replacement less likely
      ;; maybe make text behind buttons more unique
      (goto-char (point-min))
      (progn
	(while (search-forward "     " nil t)
	  (replace-match "" nil t)))
      (progn
	(while (search-forward "     " nil t)
	  (replace-match "" nil t))))))

;;;###autoload
(define-minor-mode org-inline-clocking-buttons-mode
  "Minor mode to add clocking buttons to org headings inline."
  :lighter nil
  (if org-inline-clocking-buttons-mode
      (progn
	(add-hook 'org-clock-in-hook #'org-inline-clocking-buttons-add-clock-out-button-to-right-of-heading)
	(add-hook 'org-clock-out-hook #'org-inline-clocking-buttons-add-clock-in-button-to-right-of-heading)
	(org-inline-add-play-button-to-todo-headings))
    (progn
      (org-inline-clocking-buttons-remove-org-inline-clock-button-overlays)
      (remove-hook 'org-clock-in-hook #'org-inline-clocking-buttons-add-clock-out-button-to-right-of-heading)
      (remove-hook 'org-clock-out-hook #'org-inline-clocking-buttons-add-clock-in-button-to-right-of-heading))))

(provide 'org-inline-clocking-buttons-mode)

(provide 'org-inline-clocking-buttons)

;;; org-inline-clocking-buttons.el ends here
