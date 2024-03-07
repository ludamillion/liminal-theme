;;; liminal-theme.el --- Liminal theme -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Free Software Foundation, Inc.

;; Maintainer: Luke D. Inglis <ld.inglis@gmail.com>
;; URL: https://github.com/ludamillion/nano-theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: theme, dark, light

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Liminal theme is a consistent theme that comes in light and
;; dark variations with colors derived from Protesilaos Stavrou's
;; lovely Modus themes and overall.
;;
;; It is a fork of Nicolas Rougier's N Λ N O theme and aims to
;; mostly follow the same design goals.
;;
;; References
;; - https://protesilaos.com/emacs/modus-themes
;; - https://github.com/rougier/nano-theme
;;
;; A theme is fully defined by a set of (1+6) faces as explained in
;; "On the Design of Text Editors" / https://arxiv.org/abs/2008.06030
;;
;; Priciples
;;
;; - Default face is the face for regular information.
;;
;; - Critical face is for information that requires immediate action.
;;
;;     It should be of high constrast when compared to other
;;     faces.  This can be realized (for example) by setting an intense
;;     background color, typically a shade of red.  It must be used
;;     scarcely.
;;
;; - Popout face is used for information that needs attention.
;;
;;     To achieve such effect, the hue of the face has to be
;;     sufficiently different from other faces such that it attracts
;;     attention through the popout effect.
;;
;; - Strong face is used for information of a structural nature.
;;
;;     It has to be the same color as the default color and only the
;;     weight differs by one level (e.g., light/regular or
;;     regular/bold).  It is generally used for titles, keywords,
;;     directory, etc.
;;
;; - Salient face is used for information that are important.
;;
;;     To suggest the information is of the same nature but important,
;;     the face uses a different hue with approximately the same
;;     intensity as the default face.  This is typically used for
;;     links.
;;
;; - Faded face is for information that are less important.
;;
;;     It is made by using the same hue as the default but with a
;;     lesser intensity than the default.  It can be used for
;;     comments, secondary information and also replace
;;     italic (which is generally abused anyway).
;;
;; - Subtle face is used to suggest a physical area on the screen.
;;
;;     It is important to not disturb too strongly the reading of
;;     information and this can be made by setting a very light
;;     background color that is barely perceptible.
;;
;;
;; Usage example:
;;
;; You can use the theme as a regular theme or you can call
;; (liminal-light) / (liminal-dark) explicitely to install the light or dark
;; version.
;;
;; With GUI, you can mix frames with light and dark mode.  Just call
;; (liminal-new-frame 'light) or (liminal-new-frame 'dark)
;;
;; Optionally, you can use (liminal-mode) to setup recommended settings for
;; the theme.  Be careful since it will modify your configuration and
;; requires a set of specific fonts.  This needs to be called before
;; setting the theme
;;
;; Recommended font is "Noto Sans Mono" or "Noto Sans Mono Nerd" if you want
;; to benefit from all the fancy glyphs.  See https://www.nerdfonts.com.

;;; NEWS:

;; Version 0.3.3
;; - Removed debug message
;; - Minor changes in agenda

;; Version 0.3.2
;; - Fix magit diff whitespace
;; - Update mu4e faces (1.8.x release)
;; - Added rounded corners for emacs-plus@29

;; Version 0.3.1
;; - Modified vertico and org modes
;; - Added imenu-list, ansi-color and SHR faces

;; Version 0.3.0
;; - Added italic (Victor Mono)
;; - Less salient critical face
;; - Added orderles, marginalia & corfu faces

;; Version 0.2.1
;; - Added nano-modeline faces

;; Version 0.2
;; - Split light / dark themes properly
;; - Added a nano-new-frame function
;;
;; Version 0.1
;; - Submission to ELPA

;;; Code:
(package-initialize)

(require 'disp-table)
(require 'cl-macs)

(add-to-list 'load-path ".")

(require 'liminal-custom-defs)
(require 'liminal-theme-utils)

(defun liminal-new-frame (&optional mode)
  "Create a new frame in light or dark MODE."

  (interactive)
  (let ((mode (or mode (frame-parameter nil 'background-mode)))
        (background-mode frame-background-mode)
        (selected-frame (selected-frame))
        (liminal-theme-frame-only (make-frame-command)))
    (liminal-theme nil mode)))

(defun liminal-frame-list-advice-selected (_frames)
  "Return a new frame list containing only the selected frame."
  (list (selected-frame)))

(defun liminal-frame-list-advice-normal (frames)
  "Return as a list the subset of FRAMES containing only non-standalone frames."
  (seq-filter (lambda (f) (not (frame-parameter f 'liminal-theme-standalone))) frames))

(defun liminal-frame-enable (mode)
  "Enable liminal MODE for the current frame only."
  (let ((frame (selected-frame))
        (frame-background-mode mode))
    (set-frame-parameter frame 'liminal-theme-standalone mode)
    (frame-set-background-mode frame)
    (advice-add 'frame-list :filter-return #'liminal-frame-list-advice-selected)
    (enable-theme 'liminal)
    (advice-remove 'frame-list #'liminal-frame-list-advice-selected)))

(defun liminal-frame-dark ()
  "Load the liminal dark theme on current frame."
  (interactive)
  (liminal-frame-enable 'dark))

(defun liminal-frame-light ()
  "Load the liminal light theme on current frame."
  (interactive)
  (liminal-frame-enable 'light))

(defun liminal-theme-frame-toggle ()
  "Toggle theme on current frame only."
  (interactive)
  (if (eq (or (frame-parameter (selected-frame) 'liminal-theme-standalone) frame-background-mode) 'light)
      (liminal-frame-dark)
    (liminal-frame-light)))

(defun liminal-enable (mode)
  "Enable liminal MODE all other frames."
  (advice-add 'frame-list :filter-return #'liminal-frame-list-advice-normal)
  (liminal-theme 'liminal mode)
  (enable-theme 'liminal)
  (advice-remove 'frame-list #'liminal-frame-list-advice-normal))

(defun liminal-dark ()
  "Load the liminal dark theme on current frame."
  (interactive)
  (liminal-enable 'dark))

(defun liminal-light ()
  "Load the liminal light theme on current frame."
  (interactive)
  (liminal-enable 'light))

(defvar liminal-theme-phase 'light)

(defun liminal-theme-toggle ()
  "Toggle theme on all frames."
  (interactive)
  (if (eq liminal-theme-phase 'light)
      (liminal-dark)
    (liminal-light)))

(defun liminal-theme--dark?light (dark light)
  "Determine whether theme is in DARK or LIGHT mode."

  (if (eq liminal-theme-phase 'light)
      light
    dark))
(defalias '--d?l #'liminal-theme--dark?light)

(defun liminal-theme (theme phase)
  "Apply the liminal THEME according to PHASE which can be \\='dark or \\='light."

  ;;; Set the frame background to match/guide the theme color.
  (setq frame-background-mode phase)
  (setq liminal-theme-phase phase)

  ;;; Wrap face customization in color palette definitions
  (let ((foreground (--d?l liminal-white      liminal-black))
        (background (--d?l liminal-black      liminal-white))
        (highlight  (--d?l liminal-off-black  liminal-off-white))
        (critical   (--d?l liminal-dark-red   liminal-light-red))
        (salient    (--d?l liminal-dark-blue  liminal-light-blue))
        (strong     (--d?l liminal-white      liminal-black))
        (popout     (--d?l liminal-dark-green liminal-light-green))
        (faded      (--d?l liminal-dark-grey  liminal-light-grey)))
        
    ;;; Font configuration
    (when liminal-font-stack
      (custom-theme-set-faces
       theme
       `(default ((t (:foreground ,foreground
                                  :background ,background
                                  :weight     ,(face-attribute 'liminal-mono :weight)
                                  :height     ,(* liminal-font-size 10)
                                  :family     ,(face-attribute 'liminal-mono :family)))))
       
       `(italic ((t (:foreground ,foreground
                                 :background ,background
                                 :weight     ,(face-attribute 'liminal-italic :weight)
                                 :height     ,(* liminal-font-size 10)
                                 :slant      ,(face-attribute 'liminal-italic :slant)
                                 :family     ,(face-attribute 'liminal-italic :family)))))
       
       `(liminal-strong ((t (:weight semi-bold ))))

       `(variable-pitch ((t (:weight ,(face-attribute 'liminal-sans :weight)
                                     :height ,(* liminal-font-size 10)
                                     :family ,(face-attribute 'liminal-sans :family)))))))

    (unless liminal-fonts-use
      (custom-theme-set-faces
       theme
       `(default ((t (:foreground ,foreground))))
       `(liminal-strong ((t (:weight bold))))))

    ;;; End font configuration

    ;;; General face configuration
    (custom-theme-set-faces
     theme

     ;; --- Base face definitions ---------------------------------------------------------
     `(default   ((t (:background ,background :foreground ,foreground))))
     `(cursor    ((t (:foreground ,background :background ,foreground))))
     `(mouse     ((t (:foreground ,foreground :background ,background))))
     `(highlight ((t (:background ,highlight))))
                                         
     `(liminal-default   ((t (:foreground ,foreground))))
     `(liminal-default-i ((t (:foreground ,background :background ,foreground))))

     `(liminal-faded     ((t (:foreground ,faded))))
     `(liminal-subtle    ((t (:background ,faded))))

     `(liminal-salient   ((t (:foreground ,salient))))
     `(liminal-salient-i ((t (:foreground ,background :background ,salient))))

     `(liminal-popout    ((t (:foreground ,popout))))
     `(liminal-popout-i  ((t (:foreground ,background :background ,popout))))

     `(liminal-critical   ((t (:foreground ,critical :weight normal))))
     `(liminal-critical-i ((t (:foreground ,background
                                           :background ,critical
                                           :weight normal))))

     `(liminal-strong     ((t (:inherit liminal-default   :weight semi-bold))))
     `(liminal-strong-i   ((t (:inherit liminal-default-i :weight semi-bold))))

    ;; --- Window divider ----------------------------------------------
    (if liminal-window-divider-show
        (custom-theme-set-faces
         theme
         `(window-divider  ((t (:foreground ,faded))))
         `(vertical-border ((t (:foreground ,faded)))))
      
      (custom-theme-set-faces
       theme
       `(window-divider  ((t (:foreground ,background))))
       `(vertical-border ((t (:foreground ,background))))))
    
    (custom-theme-set-faces
     theme
     '(window-divider-first-pixel ((t (:inherit window-divider))))
     '(window-divider-last-pixel  ((t (:inherit window-divider)))))

     ;;; Start theming actual UI faces
     
     ;; --- Structural ---------------------------------------------------
     '(bold                        ((t (:inherit liminal-strong))))
     '(italic                      ((t (:inherit liminal-faded :slant italic))))
     '(bold-italic                 ((t (:inherit liminal-strong))))
     '(region                      ((t (:inherit highlight :distant-foreground unspecified))))
     '(fringe                      ((t (:inherit liminal-faded))))
     '(hl-line                     ((t (:inherit highlight))))
     '(link                        ((t (:inherit liminal-salient))))
     '(fixed-pitch                 ((t (:inherit default))))
     '(fixed-pitch-serif           ((t (:inherit default))))

     ;; --- Semantic -----------------------------------------------------
     '(shadow                        ((t (:inherit liminal-faded))))
     '(success                       ((t (:inherit liminal-salient))))
     '(warning                       ((t (:inherit liminal-popout))))
     '(error                         ((t (:inherit liminal-critical))))
     '(match                         ((t (:inherit liminal-popout))))

     ;; --- General ------------------------------------------------------
     '(buffer-menu-buffer            ((t (:inherit liminal-strong))))
     '(minibuffer-prompt             ((t (:inherit liminal-strong))))
     '(isearch                       ((t (:inherit liminal-salient-i))))
     '(isearch-fail                  ((t (:inherit liminal-faded))))
     '(show-paren-match              ((t (:inherit (liminal-salient liminal-strong)))))
     '(show-paren-mismatch           ((t (:inherit (liminal-critical-i liminal-strong)))))
     '(lazy-highlight                ((t (:inherit liminal-subtle))))
     '(trailing-whitespace           ((t (:inherit liminal-faded))))
     '(secondary-selection           ((t (:inherit liminal-subtle))))
     '(completions-annotations       ((t (:inherit liminal-subtle-i))))
     '(completions-common-part       ((t (:inherit liminal-strong))))
     '(completions-first-difference  ((t (:inherit liminal-default))))
     '(tooltip                       ((t (:inherit liminal-subtle))))
     '(read-multiple-choice-face     ((t (:inherit liminal-strong))))
     '(nobreak-hyphen                ((t (:inherit liminal-popout))))
     '(nobreak-space                 ((t (:inherit liminal-popout))))
     '(help-argument-name            ((t (:inherit liminal-faded))))
     '(tabulated-list-fake-header    ((t (:inherit liminal-strong))))
     '(tool-bar                      ((t (:inherit liminal-faded-i))))

     ;; --- TTY faces ----------------------------------------------------
     '(tty-menu-disabled-face        ((t (:inherit liminal-faded-i))))
     '(tty-menu-enabled-face         ((t (:inherit liminal-default-i))))
     '(tty-menu-selected-face        ((t (:inherit liminal-salient-i))))

     ;; --- Tab bar ------------------------------------------------------
     '(tab-bar                       ((t (:inherit default))))
     '(tab-bar-tab                   ((t (:inherit default))))
     '(tab-bar-tab-inactive          ((t (:inherit liminal-faded))))
     '(tab-line                      ((t (:inherit default))))

     ;; --- Line numbers -------------------------------------------------
     '(line-number                  ((t (:inherit liminal-faded))))
     '(line-number-current-line     ((t (:inherit nil))))
     `(line-number-major-tick       ((t (:inherit liminal-faded))))
     '(line-number-minor-tick       ((t (:inherit liminal-faded))))

     ;; --- Font lock ----------------------------------------------------
     '(font-lock-comment-face        ((t (:inherit liminal-faded))))
     '(font-lock-doc-face            ((t (:inherit liminal-faded))))
     '(font-lock-string-face         ((t (:inherit liminal-faded))))
     '(font-lock-constant-face       ((t (:inherit liminal-salient))))
     '(font-lock-warning-face        ((t (:inherit liminal-popout))))
     '(font-lock-function-name-face  ((t (:inherit liminal-strong))))
     '(font-lock-variable-name-face  ((t (:inherit (liminal-strong liminal-salient)))))
     '(font-lock-builtin-face        ((t (:inherit liminal-salient))))
     '(font-lock-type-face           ((t (:inherit liminal-default))))
     '(font-lock-keyword-face        ((t (:inherit liminal-salient))))

     ;; --- Custom edit --------------------------------------------------
     '(widget-field                  ((t (:inherit liminal-subtle))))
     '(widget-button                 ((t (:inherit liminal-strong))))
     '(widget-single-line-field      ((t (:inherit liminal-subtle))))
     '(custom-group-subtitle         ((t (:inherit liminal-strong))))
     '(custom-group-tag              ((t (:inherit liminal-strong))))
     '(custom-group-tag-1            ((t (:inherit liminal-strong))))
     '(custom-comment                ((t (:inherit liminal-faded))))
     '(custom-comment-tag            ((t (:inherit liminal-faded))))
     '(custom-changed                ((t (:inherit liminal-salient))))
     '(custom-modified               ((t (:inherit liminal-salient))))
     '(custom-face-tag               ((t (:inherit liminal-strong))))
     '(custom-variable-tag           ((t (:inherit liminal-strong))))
     '(custom-invalid                ((t (:inherit liminal-popout))))
     '(custom-visibility             ((t (:inherit liminal-salient))))
     '(custom-state                  ((t (:inherit liminal-salient))))
     '(custom-link                   ((t (:inherit liminal-salient))))
     '(custom-variable-obsolete      ((t (:inherit liminal-faded))))

     ;; --- Whitespace Mode ----------------------------------------------
     '(whitespace-trailing ((t (:inherit liminal-critical-i))))
     '(whitespace-line     ((t (:inherit liminal-critical))))

     ;; --- Compilation --------------------------------------------------
     '(compilation-error ((t (:inherit liminal-critical))))
     '(compilation-info ((t (:inherit liminal-default))))
     '(compilation-warning ((t (:inherit liminal-popout))))
     '(compilation-line-number ((t (:inherit liminal-default))))
     '(compilation-column-number ((t (:inherit liminal-default))))
     '(compilation-mode-line-run ((t (:inherit liminal-default-i))))
     '(compilation-mode-line-exit ((t (:inherit liminal-default-i))))
     '(compilation-mode-line-fail ((t (:inherit liminal-critical))))
     
     ;; --- Packages -----------------------------------------------------
     '(package-description            ((t (:inherit liminal-default))))
     '(package-help-section-name      ((t (:inherit liminal-default))))
     '(package-name                   ((t (:inherit liminal-salient))))
     '(package-status-avail-obso      ((t (:inherit liminal-faded))))
     '(package-status-available       ((t (:inherit liminal-default))))
     '(package-status-built-in        ((t (:inherit liminal-salient))))
     '(package-status-dependency      ((t (:inherit liminal-salient))))
     '(package-status-disabled        ((t (:inherit liminal-faded))))
     '(package-status-external        ((t (:inherit liminal-default))))
     '(package-status-held            ((t (:inherit liminal-default))))
     '(package-status-incompat        ((t (:inherit liminal-faded))))
     '(package-status-installed       ((t (:inherit liminal-salient))))
     '(package-status-new             ((t (:inherit liminal-default))))
     '(package-status-unsigned        ((t (:inherit liminal-default))))

     ;; --- Info ---------------------------------------------------------
     '(info-node                      ((t (:inherit liminal-strong))))
     '(info-menu-header               ((t (:inherit liminal-strong))))
     '(info-header-node               ((t (:inherit liminal-default))))
     '(info-index-match               ((t (:inherit liminal-salient))))
     '(Info-quoted                    ((t (:inherit liminal-faded))))
     '(info-title-1                   ((t (:inherit liminal-strong))))
     '(info-title-2                   ((t (:inherit liminal-strong))))
     '(info-title-3                   ((t (:inherit liminal-strong))))
     '(info-title-4                   ((t (:inherit liminal-strong))))

     ;; --- Helpful ------------------------------------------------------
     '(helpful-heading                ((t (:inherit liminal-strong))))
     
     ;; --- liminal agenda ---------------------------------------------------------
     '(liminal-agenda-button               ((t (:inherit (liminal-faded)))))
     '(liminal-agenda-day-name             ((t (:inherit (liminal-faded)))))
     '(liminal-agenda-default              ((t (:inherit (liminal-default)))))
     '(liminal-agenda-holidays             ((t (:inherit (liminal-faded)))))
     '(liminal-agenda-month-name           ((t (:inherit (liminal-strong)))))
     '(liminal-agenda-mouse                ((t (:inherit (highlight)))))
     '(liminal-agenda-outday               ((t (:inherit (liminal-subtle-i)))))
     '(liminal-agenda-selected             ((t (:inherit (liminal-default-i)))))
     '(liminal-agenda-selected-today       ((t (:inherit (liminal-popout-i liminal-strong)))))
     '(liminal-agenda-today                ((t (:inherit (liminal-popout liminal-strong)))))
     '(liminal-agenda-weekend              ((t (:inherit (liminal-faded)))))

     ;; --- EPA ----------------------------------------------------------
     '(epa-field-body                 ((t (:inherit liminal-default))))
     '(epa-field-name                 ((t (:inherit liminal-strong))))
     '(epa-mark                       ((t (:inherit liminal-salient))))
     '(epa-string                     ((t (:inherit liminal-popout))))
     '(epa-validity-disabled          ((t (:inherit liminal-faded))))
     '(epa-validity-high              ((t (:inherit liminal-strong))))
     '(epa-validity-medium            ((t (:inherit liminal-default))))
     '(epa-validity-low               ((t (:inherit liminal-faded))))

     ;; --- Popup --------------------------------------------------------
     '(popup-face                       ((t (:inherit highlight))))
     '(popup-isearch-match              ((t (:inherit liminal-popout))))
     '(popup-menu-face                  ((t (:inherit liminal-subtle))))
     '(popup-menu-mouse-face            ((t (:inherit liminal-faded-i))))
     '(popup-menu-selection-face        ((t (:inherit liminal-salient-i))))
     '(popup-menu-summary-face          ((t (:inherit liminal-faded))))
     '(popup-scroll-bar-background-face ((t (:inherit liminal-subtle))))
     '(popup-scroll-bar-foreground-face ((t (:inherit liminal-subtle))))
     '(popup-summary-face               ((t (:inherit liminal-faded))))
     '(popup-tip-face                   ((t (:inherit liminal-popout-i))))

     ;; --- Diff ---------------------------------------------------------
     '(diff-header                    ((t (:inherit liminal-faded))))
     '(diff-file-header               ((t (:inherit liminal-strong))))
     '(diff-context                   ((t (:inherit liminal-default))))
     '(diff-removed                   ((t (:inherit liminal-faded))))
     '(diff-changed                   ((t (:inherit liminal-popout))))
     '(diff-added                     ((t (:inherit liminal-salient))))
     '(diff-refine-added              ((t (:inherit (liminal-salient
                                                     liminal-strong)))))
     '(diff-refine-changed            ((t (:inherit liminal-popout))))
     '(diff-refine-removed            ((t (:inherit liminal-faded
                                                    :strike-through t))))
     ;; --- Vertico --------------------------------------------------------
     '(vertico-current                       ((t (:inherit highlight))))
     '(vertico-group-separator               ((t (:inherit liminal-faded))))
     '(vertico-group-title                   ((t (:inherit liminal-faded))))
     '(vertico-multiline                     ((t (:inherit liminal-faded))))
     
     ;; --- Citar --------------------------------------------------------
     '(citar                          ((t (:inherit liminal-faded))))
     '(citar-highlight                ((t (:inherit liminal-default))))

     ;; --- Corfu --------------------------------------------------------
     '(corfu-annotations              ((t (:inherit liminal-faded))))
     '(corfu-bar                      ((t (:inherit liminal-default-i))))
     '(corfu-border                   ((t (:inherit liminal-default-i))))
     '(corfu-current                  ((t (:inherit highlight))))
     '(corfu-default                  ((t (:inherit liminal-subtle))))
     '(corfu-deprecated               ((t (:inherit liminal-faded))))
     '(corfu-echo                     ((t (:inherit liminal-faded))))

     ;; --- Orderless ----------------------------------------------------
     '(orderless-match-face-0         ((t (:inherit (liminal-popout
                                                     liminal-strong)))))
     '(orderless-match-face-1         ((t (:inherit (liminal-strong)))))
     '(orderless-match-face-2         ((t (:inherit (liminal-strong)))))
     '(orderless-match-face-3         ((t (:inherit (liminal-strong)))))

     ;; --- Message ------------------------------------------------------
     '(message-cited-text-1           ((t (:inherit liminal-faded))))
     '(message-cited-text-2           ((t (:inherit liminal-faded))))
     '(message-cited-text-3           ((t (:inherit liminal-faded))))
     '(message-cited-text-4           ((t (:inherit liminal-faded))))
     '(message-cited-text             ((t (:inherit liminal-faded))))
     '(message-header-cc              ((t (:inherit liminal-default))))
     '(message-header-name            ((t (:inherit liminal-strong))))
     '(message-header-newsgroups      ((t (:inherit liminal-default))))
     '(message-header-other           ((t (:inherit liminal-default))))
     '(message-header-subject         ((t (:inherit liminal-salient))))
     '(message-header-to              ((t (:inherit liminal-salient))))
     '(message-header-xheader         ((t (:inherit liminal-default))))
     '(message-mml                    ((t (:inherit liminal-popout))))
     '(message-separator              ((t (:inherit liminal-faded))))

     ;; --- Outline ------------------------------------------------------
     '(outline-1                      ((t (:inherit liminal-strong))))
     '(outline-2                      ((t (:inherit liminal-strong))))
     '(outline-3                      ((t (:inherit liminal-strong))))
     '(outline-4                      ((t (:inherit liminal-strong))))
     '(outline-5                      ((t (:inherit liminal-strong))))
     '(outline-6                      ((t (:inherit liminal-strong))))
     '(outline-7                      ((t (:inherit liminal-strong))))
     '(outline-8                      ((t (:inherit liminal-strong))))

     ;; --- Fly spell ----------------------------------------------------
     '(flyspell-duplicate             ((t (:inherit liminal-popout
                                                    :underline t))))
     '(flyspell-incorrect             ((t (:inherit liminal-popout
                                                    :underline t))))

     ;; --- Org agenda ---------------------------------------------------
     '(org-agenda-calendar-event      ((t (:inherit liminal-default))))
     '(org-agenda-calendar-sexp       ((t (:inherit liminal-salient))))
     '(org-agenda-clocking            ((t (:inherit liminal-faded))))
     '(org-agenda-column-dateline     ((t (:inherit liminal-faded))))
     '(org-agenda-current-time        ((t (:inherit (liminal-strong
                                                     liminal-salient)))))
     '(org-agenda-date                ((t (:inherit liminal-strong))))
     '(org-agenda-date-today          ((t (:inherit (liminal-salient
                                                     liminal-strong)))))
     '(org-agenda-date-weekend        ((t (:inherit liminal-faded))))
     '(org-agenda-diary               ((t (:inherit liminal-faded))))
     '(org-agenda-dimmed-todo-face    ((t (:inherit liminal-faded))))
     '(org-agenda-done                ((t (:inherit liminal-faded))))
     '(org-agenda-filter-category     ((t (:inherit liminal-faded))))
     '(org-agenda-filter-effort       ((t (:inherit liminal-faded))))
     '(org-agenda-filter-regexp       ((t (:inherit liminal-faded))))
     '(org-agenda-filter-tags         ((t (:inherit liminal-faded))))
     '(org-agenda-property-face       ((t (:inherit liminal-faded))))
     '(org-agenda-restriction-lock    ((t (:inherit liminal-faded))))
     '(org-agenda-structure           ((t (:inherit liminal-strong))))

     ;; --- Org ----------------------------------------------------------
     '(org-archived                            ((t (:inherit liminal-faded))))
     '(org-block                               ((t (:inherit liminal-faded))))
     `(org-block-begin-line                    ((t (:inherit liminal-faded
                                                             :underline ,(face-background 'liminal-subtle)))))
     `(org-block-end-line                      ((t (:inherit liminal-faded
                                                             :overline ,(face-background 'liminal-subtle)))))
     '(org-checkbox                            ((t (:inherit liminal-faded))))
     '(org-checkbox-statistics-done            ((t (:inherit liminal-faded))))
     '(org-checkbox-statistics-todo            ((t (:inherit liminal-faded))))
     '(org-clock-overlay                       ((t (:inherit liminal-faded))))
     '(org-code                                ((t (:inherit liminal-salient))))
     '(org-column                              ((t (:inherit liminal-faded))))
     '(org-column-title                        ((t (:inherit liminal-faded))))
     '(org-date                                ((t (:inherit liminal-faded))))
     '(org-date-selected                       ((t (:inherit liminal-faded))))
     '(org-default                             ((t (:inherit liminal-faded))))
     '(org-document-info                       ((t (:inherit liminal-faded))))
     '(org-document-info-keyword               ((t (:inherit liminal-faded))))
     '(org-document-title                      ((t (:inherit liminal-faded))))
     '(org-done                                ((t (:inherit liminal-faded))))
     '(org-drawer                              ((t (:inherit liminal-faded))))
     '(org-ellipsis                            ((t (:inherit liminal-faded))))
     '(org-footnote                            ((t (:inherit liminal-faded))))
     '(org-formula                             ((t (:inherit liminal-faded))))
     '(org-headline-done                       ((t (:inherit liminal-faded))))
     ;; '(org-hide                                ((t (:inherit liminal-subtle-i))))
     ;; '(org-indent                              ((t (:inherit liminal-subtle-i))))
     '(org-latex-and-related                   ((t (:inherit liminal-faded))))
     '(org-level-1                             ((t (:inherit liminal-strong))))
     '(org-level-2                             ((t (:inherit liminal-strong))))
     '(org-level-3                             ((t (:inherit liminal-strong))))
     '(org-level-4                             ((t (:inherit liminal-strong))))
     '(org-level-5                             ((t (:inherit liminal-strong))))
     '(org-level-6                             ((t (:inherit liminal-strong))))
     '(org-level-7                             ((t (:inherit liminal-strong))))
     '(org-level-8                             ((t (:inherit liminal-strong))))
     '(org-link                                ((t (:inherit liminal-salient))))
     '(org-list-dt                             ((t (:inherit liminal-faded))))
     '(org-macro                               ((t (:inherit liminal-faded))))
     '(org-meta-line                           ((t (:inherit liminal-faded))))
     '(org-mode-line-clock                     ((t (:inherit liminal-faded))))
     '(org-mode-line-clock-overrun             ((t (:inherit liminal-faded))))
     '(org-priority                            ((t (:inherit liminal-faded))))
     '(org-property-value                      ((t (:inherit liminal-faded))))
     '(org-quote                               ((t (:inherit liminal-faded))))
     '(org-scheduled                           ((t (:inherit liminal-faded))))
     '(org-scheduled-previously                ((t (:inherit liminal-faded))))
     '(org-scheduled-today                     ((t (:inherit liminal-faded))))
     '(org-sexp-date                           ((t (:inherit liminal-faded))))
     '(org-special-keyword                     ((t (:inherit liminal-faded))))
     '(org-table                               ((t (:inherit liminal-faded))))
     '(org-tag                                 ((t (:inherit liminal-popout))))
     '(org-tag-group                           ((t (:inherit liminal-faded))))
     '(org-target                              ((t (:inherit liminal-faded))))
     '(org-time-grid                           ((t (:inherit liminal-faded))))
     '(org-todo                                ((t (:inherit liminal-salient))))
     '(org-upcoming-deadline                   ((t (:inherit liminal-popout))))
     '(org-verbatim                            ((t (:inherit liminal-popout))))
     '(org-verse                               ((t (:inherit liminal-faded))))
     '(org-warning                             ((t (:inherit liminal-popout))))

     ;; --- Marginalia ---------------------------------------------------
     '(marginalia-archive                     ((t (:inherit liminal-faded))))
     '(marginalia-char                        ((t (:inherit liminal-faded))))
     '(marginalia-date                        ((t (:inherit liminal-faded))))
     '(marginalia-documentation               ((t (:inherit liminal-faded))))
     '(marginalia-file-name                   ((t (:inherit liminal-faded))))
     '(marginalia-file-owner                  ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-dir               ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-exec              ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-link              ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-no                ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-other             ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-rare              ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-read              ((t (:inherit liminal-faded))))
     '(marginalia-file-priv-write             ((t (:inherit liminal-faded))))
     '(marginalia-function                    ((t (:inherit liminal-faded))))
     '(marginalia-installed                   ((t (:inherit liminal-faded))))
     '(marginalia-key                         ((t (:inherit liminal-faded))))
     '(marginalia-lighter                     ((t (:inherit liminal-faded))))
     '(marginalia-list                        ((t (:inherit liminal-faded))))
     '(marginalia-mode                        ((t (:inherit liminal-faded))))
     '(marginalia-modified                    ((t (:inherit liminal-faded))))
     '(marginalia-null                        ((t (:inherit liminal-faded))))
     '(marginalia-number                      ((t (:inherit liminal-faded))))
     '(marginalia-off                         ((t (:inherit liminal-faded))))
     '(marginalia-on                          ((t (:inherit liminal-faded))))
     '(marginalia-size                        ((t (:inherit liminal-faded))))
     '(marginalia-string                      ((t (:inherit liminal-faded))))
     '(marginalia-symbol                      ((t (:inherit liminal-faded))))
     '(marginalia-true                        ((t (:inherit liminal-faded))))
     '(marginalia-type                        ((t (:inherit liminal-faded))))
     '(marginalia-value                       ((t (:inherit liminal-faded))))
     '(marginalia-version                     ((t (:inherit liminal-faded))))

     ;; --- Elfeed -------------------------------------------------------
     '(elfeed-log-date-face                    ((t (:inherit liminal-faded))))
     '(elfeed-log-info-level-face            ((t (:inherit liminal-default))))
     '(elfeed-log-debug-level-face           ((t (:inherit liminal-default))))
     '(elfeed-log-warn-level-face             ((t (:inherit liminal-popout))))
     '(elfeed-log-error-level-face            ((t (:inherit liminal-popout))))
     '(elfeed-search-tag-face                  ((t (:inherit liminal-faded))))
     '(elfeed-search-date-face                 ((t (:inherit liminal-faded))))
     '(elfeed-search-feed-face               ((t (:inherit liminal-salient))))
     '(elfeed-search-filter-face               ((t (:inherit liminal-faded))))
     '(elfeed-search-last-update-face        ((t (:inherit liminal-salient))))
     '(elfeed-search-title-face              ((t (:inherit liminal-default))))
     '(elfeed-search-tag-face                  ((t (:inherit liminal-faded))))
     '(elfeed-search-unread-count-face        ((t (:inherit liminal-strong))))
     '(elfeed-search-unread-title-face        ((t (:inherit liminal-strong))))

     ;; --- Deft --------------------------------------------------------
     '(deft-filter-string-error-face         ((t (:inherit liminal-popout))))
     '(deft-filter-string-face              ((t (:inherit liminal-default))))
     '(deft-header-face                     ((t (:inherit liminal-salient))))
     '(deft-separator-face                    ((t (:inherit liminal-faded))))
     '(deft-summary-face                      ((t (:inherit liminal-faded))))
     '(deft-time-face                       ((t (:inherit liminal-salient))))
     '(deft-title-face                       ((t (:inherit liminal-strong))))

     ;; --- imenu-list ---------------------------------------------------
     '(imenu-list-entry-face                 ((t (:inherit liminal-default))))
     '(imenu-list-entry-face-0               ((t (:inherit liminal-strong))))
     '(imenu-list-entry-face-1               ((t ( ))))
     '(imenu-list-entry-face-2               ((t ( ))))
     '(imenu-list-entry-face-3               ((t ( ))))
     '(imenu-list-entry-subalist-face-0      ((t (:inherit liminal-strong))))
     '(imenu-list-entry-subalist-face-1      ((t ( ))))
     '(imenu-list-entry-subalist-face-2      ((t ( ))))
     '(imenu-list-entry-subalist-face-3      ((t ( ))))

     ;; --- Restructured text -------------------------------------------
     '(rst-adornment                           ((t (:inherit liminal-faded))))
     '(rst-block                             ((t (:inherit liminal-default))))
     '(rst-comment                             ((t (:inherit liminal-faded))))
     '(rst-definition                        ((t (:inherit liminal-salient))))
     '(rst-directive                         ((t (:inherit liminal-salient))))
     '(rst-emphasis1                           ((t (:inherit liminal-faded))))
     '(rst-emphasis2                          ((t (:inherit liminal-strong))))
     '(rst-external                          ((t (:inherit liminal-salient))))
     '(rst-level-1                            ((t (:inherit liminal-strong))))
     '(rst-level-2                            ((t (:inherit liminal-strong))))
     '(rst-level-3                            ((t (:inherit liminal-strong))))
     '(rst-level-4                            ((t (:inherit liminal-strong))))
     '(rst-level-5                            ((t (:inherit liminal-strong))))
     '(rst-level-6                            ((t (:inherit liminal-strong))))
     '(rst-literal                           ((t (:inherit liminal-salient))))
     '(rst-reference                         ((t (:inherit liminal-salient))))
     '(rst-transition                        ((t (:inherit liminal-default))))


     ;; ---SHR ---------------------------------------------------------
     '(shr-abbreviation                    ((t (:inherit liminal-popout))))
     '(shr-h1                              ((t (:inherit liminal-strong))))
     '(shr-h2                              ((t (:inherit liminal-strong))))
     '(shr-h3                              ((t (:inherit liminal-strong))))
     '(shr-h4                              ((t (:inherit liminal-strong))))
     '(shr-h5                              ((t (:inherit liminal-strong))))
     '(shr-h6                              ((t (:inherit liminal-strong))))
     '(shr-link                           ((t (:inherit liminal-salient))))
     '(shr-selected-link      ((t (:inherit (liminal-salient liminal-subtle)))))
     '(shr-strike-through                   ((t (:inherit liminal-faded))))

     ;; --- Markdown ----------------------------------------------------
     '(markdown-blockquote-face              ((t (:inherit liminal-default))))
     '(markdown-bold-face                     ((t (:inherit liminal-strong))))
     '(markdown-code-face                    ((t (:inherit liminal-default))))
     '(markdown-comment-face                   ((t (:inherit liminal-faded))))
     '(markdown-footnote-marker-face         ((t (:inherit liminal-default))))
     '(markdown-footnote-text-face           ((t (:inherit liminal-default))))
     '(markdown-gfm-checkbox-face            ((t (:inherit liminal-default))))
     '(markdown-header-delimiter-face          ((t (:inherit liminal-faded))))
     '(markdown-header-face                   ((t (:inherit liminal-strong))))
     '(markdown-header-face-1                 ((t (:inherit liminal-strong))))
     '(markdown-header-face-2                 ((t (:inherit liminal-strong))))
     '(markdown-header-face-3                 ((t (:inherit liminal-strong))))
     '(markdown-header-face-4                 ((t (:inherit liminal-strong))))
     '(markdown-header-face-5                 ((t (:inherit liminal-strong))))
     '(markdown-header-face-6                ((t (:inherit liminal-strong))))
     '(markdown-header-rule-face             ((t (:inherit liminal-default))))
     '(markdown-highlight-face               ((t (:inherit liminal-default))))
     '(markdown-hr-face                      ((t (:inherit liminal-default))))
     '(markdown-html-attr-name-face          ((t (:inherit liminal-default))))
     '(markdown-html-attr-value-face         ((t (:inherit liminal-default))))
     '(markdown-html-entity-face             ((t (:inherit liminal-default))))
     '(markdown-html-tag-delimiter-face      ((t (:inherit liminal-default))))
     '(markdown-html-tag-name-face           ((t (:inherit liminal-default))))
     '(markdown-inline-code-face              ((t (:inherit liminal-popout))))
     '(markdown-italic-face                    ((t (:inherit liminal-faded))))
     '(markdown-language-info-face           ((t (:inherit liminal-default))))
     '(markdown-language-keyword-face        ((t (:inherit liminal-default))))
     '(markdown-line-break-face              ((t (:inherit liminal-default))))
     '(markdown-link-face                    ((t (:inherit liminal-salient))))
     '(markdown-link-title-face              ((t (:inherit liminal-default))))
     '(markdown-list-face                      ((t (:inherit liminal-faded))))
     '(markdown-markup-face                    ((t (:inherit liminal-faded))))
     '(markdown-math-face                    ((t (:inherit liminal-default))))
     '(markdown-metadata-key-face              ((t (:inherit liminal-faded))))
     '(markdown-metadata-value-face            ((t (:inherit liminal-faded))))
     '(markdown-missing-link-face            ((t (:inherit liminal-default))))
     '(markdown-plain-url-face               ((t (:inherit liminal-default))))
     '(markdown-pre-face                     ((t (:inherit liminal-default))))
     '(markdown-reference-face               ((t (:inherit liminal-salient))))
     '(markdown-strike-through-face            ((t (:inherit liminal-faded))))
     '(markdown-table-face                   ((t (:inherit liminal-default))))
     '(markdown-url-face                     ((t (:inherit liminal-salient))))

     ;; --- Magit (WIP) ---------------------------------------------------
     '(magit-blame-highlight                  ((t (:inherit (highlight)))))
     '(magit-diff-added-highlight             ((t (:inherit (highlight liminal-popout liminal-strong)))))
     '(magit-diff-base-highlight              ((t (:inherit (highlight)))))
     '(magit-diff-context-highlight           ((t (:inherit (highlight liminal-faded)))))
     '(magit-diff-file-heading-highlight      ((t (:inherit (highlight liminal-strong)))))
     '(magit-diff-hunk-heading-highlight      ((t (:inherit (liminal-default)))))
     '(magit-diff-our-highlight               ((t (:inherit (highlight)))))
     '(magit-diff-removed-highlight           ((t (:inherit (highlight liminal-critical liminal-strong)))))
     '(magit-diff-revision-summary-highlight  ((t (:inherit ()))))
     '(magit-diff-their-highlight             ((t (:inherit (highlight)))))
     '(magit-section-highlight                ((t (:inherit (highlight)))))

     '(magit-blame-heading                    ((t (:inherit (liminal-subtle liminal-strong)))))
     '(magit-diff-conflict-heading            ((t (:inherit (liminal-subtle liminal-strong)))))
     '(magit-diff-file-heading                ((t (:inherit (liminal-strong)))))
     '(magit-diff-hunk-heading                ((t (:inherit (liminal-subtle liminal-default)))))
     '(magit-diff-lines-heading               ((t (:inherit (liminal-subtle liminal-strong)))))
     '(magit-section-heading                  ((t (:inherit (liminal-salient liminal-strong)))))

     '(magit-bisect-bad                       ((t (:inherit liminal-default))))
     '(magit-bisect-good                      ((t (:inherit liminal-default))))
     '(magit-bisect-skip                      ((t (:inherit liminal-default))))
     '(magit-blame-date                       ((t (:inherit liminal-default))))
     '(magit-blame-dimmed                     ((t (:inherit liminal-default))))
     '(magit-blame-hash                       ((t (:inherit liminal-faded))))

     '(magit-blame-margin                     ((t (:inherit liminal-default))))
     '(magit-blame-name                       ((t (:inherit liminal-default))))
     '(magit-blame-summary                    ((t (:inherit liminal-default))))

     '(magit-branch-current                   ((t (:inherit (liminal-strong liminal-salient)))))
     '(magit-branch-local                     ((t (:inherit liminal-salient))))
     '(magit-branch-remote                    ((t (:inherit (liminal-salient)))))
     '(magit-branch-remote-head               ((t (:inherit (liminal-salient)))))
     '(magit-branch-upstream                  ((t (:inherit (liminal-salient)))))

     '(magit-cherry-equivalent                ((t (:inherit liminal-default))))
     '(magit-cherry-unmatched                 ((t (:inherit liminal-default))))

     '(magit-diff-added                       ((t (:inherit (highlight liminal-popout liminal-strong)))))
     '(magit-diff-base                        ((t (:inherit liminal-default))))
     '(magit-diff-context                     ((t (:inherit (highlight liminal-faded)))))
     '(magit-diff-file-heading-selection      ((t (:inherit liminal-default))))
     '(magit-diff-hunk-heading-selection      ((t (:inherit liminal-default))))
     '(magit-diff-hunk-region                 ((t (:inherit liminal-default))))
     '(magit-diff-lines-boundary              ((t (:inherit liminal-default))))
     '(magit-diff-our                         ((t (:inherit liminal-default))))
     '(magit-diff-removed                     ((t (:inherit (highlight liminal-critical liminal-strong)))))
     '(magit-diff-revision-summary            ((t (:inherit liminal-popout))))
     '(magit-diff-their                       ((t (:inherit liminal-default))))
     '(magit-diff-whitespace-warning          ((t (:inherit liminal-subtle))))
     '(magit-diffstat-added                   ((t (:inherit liminal-default))))
     '(magit-diffstat-removed                 ((t (:inherit liminal-default))))

     '(magit-dimmed                           ((t (:inherit liminal-faded))))
     '(magit-filename                         ((t (:inherit liminal-default))))
     '(magit-hash                             ((t (:inherit liminal-faded))))
     '(magit-head                             ((t (:inherit liminal-default))))
     '(magit-header-line                      ((t (:inherit liminal-default))))
     '(magit-header-line-key                  ((t (:inherit liminal-default))))
     '(magit-header-line-log-select           ((t (:inherit liminal-default))))

     '(magit-keyword                          ((t (:inherit liminal-salient))))
     '(magit-keyword-squash                   ((t (:inherit liminal-salient))))

     '(magit-log-author                       ((t (:inherit liminal-default))))
     '(magit-log-date                         ((t (:inherit liminal-default))))
     '(magit-log-graph                        ((t (:inherit liminal-default))))

     '(magit-mode-line-process                ((t (:inherit liminal-default))))
     '(magit-mode-line-process-error          ((t (:inherit liminal-critical))))

     '(magit-process-ng                       ((t (:inherit liminal-default))))
     '(magit-process-ok                       ((t (:inherit liminal-default))))

     '(magit-reflog-amend                     ((t (:inherit liminal-default))))
     '(magit-reflog-checkout                  ((t (:inherit liminal-default))))
     '(magit-reflog-cherry-pick               ((t (:inherit liminal-default))))
     '(magit-reflog-commit                    ((t (:inherit liminal-default))))
     '(magit-reflog-merge                     ((t (:inherit liminal-default))))
     '(magit-reflog-other                     ((t (:inherit liminal-default))))
     '(magit-reflog-rebase                    ((t (:inherit liminal-default))))
     '(magit-reflog-remote                    ((t (:inherit liminal-default))))
     '(magit-reflog-reset                     ((t (:inherit liminal-default))))
     '(magit-refname                          ((t (:inherit liminal-default))))
     '(magit-refname-pullreq                  ((t (:inherit liminal-default))))
     '(magit-refname-stash                    ((t (:inherit liminal-default))))
     '(magit-refname-wip                      ((t (:inherit liminal-default))))

     '(magit-section-heading-selection        ((t (:inherit liminal-default))))
     '(magit-section-secondary-heading        ((t (:inherit liminal-default))))
     '(magit-sequence-done                    ((t (:inherit liminal-default))))
     '(magit-sequence-drop                    ((t (:inherit liminal-default))))
     '(magit-sequence-exec                    ((t (:inherit liminal-default))))
     '(magit-sequence-head                    ((t (:inherit liminal-default))))
     '(magit-sequence-onto                    ((t (:inherit liminal-default))))
     '(magit-sequence-part                    ((t (:inherit liminal-default))))
     '(magit-sequence-pick                    ((t (:inherit liminal-default))))
     '(magit-sequence-stop                    ((t (:inherit liminal-default))))

     '(magit-signature-bad                    ((t (:inherit liminal-default))))
     '(magit-signature-error                  ((t (:inherit liminal-default))))
     '(magit-signature-expired                ((t (:inherit liminal-default))))
     '(magit-signature-expired-key            ((t (:inherit liminal-default))))
     '(magit-signature-good                   ((t (:inherit liminal-default))))
     '(magit-signature-revoked                ((t (:inherit liminal-default))))
     '(magit-signature-untrusted              ((t (:inherit liminal-default))))

     '(magit-tag                              ((t (:inherit liminal-strong))))

     ;; --- Transient ------------------------------------------------------
     ;; Set only faces that influence Magit.  See:
     ;; <https://github.com/rougier/nano-theme/issues/43>
     '(transient-value                        ((t (:inherit default))))

     ;; --- Objed ----------------------------------------------------------

     '(objed-hl                               ((t (:inherit highlight))))
     '(objed-mark                             ((t (:inherit liminal-subtle))))
     '(objed-mode-line                        ((t (:inherit liminal-popout-i))))

     ;; --- ANSI colors ----------------------------------------------------

     '(ansi-color-black                       ((t (:inherit liminal-default))))
     '(ansi-color-bold                         ((t (:inherit liminal-strong))))
     '(ansi-color-bright-black                 ((t (:inherit liminal-strong))))
     '(ansi-color-faint                         ((t (:inherit liminal-faded))))
     '(ansi-color-fast-blink                    ((t (:inherit liminal-faded))))
     '(ansi-color-slow-blink                    ((t (:inherit liminal-faded))))
     '(ansi-color-inverse                   ((t (:inherit liminal-default-i))))
     '(ansi-color-italic                            ((t (:inherit italic))))
     '(ansi-color-underline                     ((t (:inherit liminal-faded))))
     '(ansi-color-blue           ((t (:inherit liminal-base16-blue))))
     '(ansi-color-bright-blue    ((t (:inherit liminal-base16-blue))))
     '(ansi-color-cyan           ((t (:inherit liminal-base16-cyan))))
     '(ansi-color-bright-cyan    ((t (:inherit liminal-base16-cyan))))
     '(ansi-color-green          ((t (:inherit liminal-base16-green))))
     '(ansi-color-bright-green   ((t (:inherit liminal-base16-green))))
     '(ansi-color-magenta        ((t (:inherit liminal-base16-magenta))))
     '(ansi-color-bright-magenta ((t (:inherit liminal-base16-magenta))))
     '(ansi-color-red            ((t (:inherit liminal-base16-red))))
     '(ansi-color-bright-red     ((t (:inherit liminal-base16-red))))
     '(ansi-color-white          ((t (:inherit liminal-subtle))))
     '(ansi-color-bright-white   ((t (:inherit default))))
     '(ansi-color-yellow         ((t (:inherit liminal-base16-yellow))))
     '(ansi-color-bright-yellow  ((t (:inherit liminal-base16-yellow))))

     ;; --- vterm colors ----------------------------------------------------

     '(vterm-color-black          ((t (:inherit liminal-default))))
     '(vterm-color-blue           ((t (:inherit liminal-base16-blue))))
     '(vterm-color-bright-blue    ((t (:inherit liminal-base16-blue))))
     '(vterm-color-cyan           ((t (:inherit liminal-base16-cyan))))
     '(vterm-color-bright-cyan    ((t (:inherit liminal-base16-cyan))))
     '(vterm-color-green          ((t (:inherit liminal-base16-green))))
     '(vterm-color-bright-green   ((t (:inherit liminal-base16-green))))
     '(vterm-color-magenta        ((t (:inherit liminal-base16-magenta))))
     '(vterm-color-bright-magenta ((t (:inherit liminal-base16-magenta))))
     '(vterm-color-red            ((t (:inherit liminal-base16-red))))
     '(vterm-color-bright-red     ((t (:inherit liminal-base16-red))))
     '(vterm-color-white          ((t (:inherit liminal-subtle))))
     '(vterm-color-bright-white   ((t (:inherit liminal-default-i))))
     '(vterm-color-yellow         ((t (:inherit liminal-base16-yellow))))
     '(vterm-color-bright-yellow  ((t (:inherit liminal-base16-yellow))))

     ;; --- Terminal ----------------------------------------------------
     '(term-bold          ((t (:inherit liminal-strong))))
     '(term-color-black   ((t (:inherit default))))
     '(term-color-blue    ((t (:inherit liminal-base16-blue))))
     '(term-color-cyan    ((t (:inherit liminal-base16-cyan))))
     '(term-color-green   ((t (:inherit liminal-base16-green))))
     '(term-color-red     ((t (:inherit liminal-base16-red))))
     '(term-color-magenta ((t (:inherit liminal-base16-magenta))))
     '(term-color-yellow  ((t (:inherit liminal-base16-yellow)))))
    
    (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                          " *Minibuf-1*" " *Echo Area 1*"))
      (when (get-buffer buffer)
        (with-current-buffer buffer
          (face-remap-add-relative 'default 'liminal-faded))))
    (advice-remove 'frame-list #'liminal-frame-list-advice-selected)
    ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'liminal-theme-support)
;;; liminal-theme-support.el ends here
