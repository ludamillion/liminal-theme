;;; liminal-theme-utils.el --- Custom/Face defs for liminal theme -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defcustom liminal-manage-cursor nil
  "Non-nil opts into Liminal's preferred cursor settings."
  :type 'boolean
  :group 'liminal-ui)

(defcustom liminal-manage-fonts nil
  "Non-nil opts into Liminal's preferred font settings."
  :type 'boolean
  :group 'liminal-ui)

(defcustom liminal-font-stack nil
  "Non-nil opts into using the Liminal font stack."
  :type 'boolean
  :group 'liminal-ui)

(defcustom liminal-manage-ui t
  "Non-nil opts into Liminal's preferred UI settings."
  :type 'boolean
  :group 'liminal-ui)

(defcustom liminal-manage-ux nil
  "Non-nil opts into Liminal's preferred UX settings."
  :type 'boolean
  :group 'liminal-ui)

(defun liminal-cursor-settings ()
  "Apply the preferred Liminal cursor settings.

Sets the cursor to a horizontal bar 2 pixels high and hides it in
all but the active window."

  (setq cursor-in-non-selected-windows nil)
  (setq-default cursor-in-non-selected-windows nil)

  (setq-default cursor-type '(hbar .  2))
  (blink-cursor-mode -1))

(defun liminal-font-settings ()
  "Apply the preferred Liminal font settings.

Controls various settings related both directly to font faces and
also to how text is displayed visually.

- Allow the maximum level of font lock decoration.
- Set line spacing to 1 pixel.
- Replace the default glyphs for indicating continuation and wrqap.
- If available use the Symbols Mono font for the Unicode private use area."

  (setq font-lock-maximum-decoration t
        line-spacing 1)

  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?… 'liminal-faded))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?- 'liminal-faded))

  (let ((symbols-nerd (font-spec :name "Symbols Nerd Font Mono" :weight 'regular)))
    (if (find-font symbols-nerd)
        (set-fontset-font t '(#xe000 . #xffdd) symbols-nerd)
      (message "Symbols Mono Nerd font has not been found on your system")))

  (setq liminal-font-stack t))

(defun liminal-ui-settings ()
  "Apply the preferred Liminal settings for Emacs' UI elements.

- Set default frame attributes
- Turn off frame titles.
- Do not indicate empty lines.
- Show trailing whitespace by default.
- Turn off scroll bar.
- Turn off tool bar.
- Turn off tooltips.
- Enable window divider mode with preferred settings."

  (setq frame-title-format nil
        indicate-empty-lines nil
        show-trailing-whitespace t)

  (setq default-frame-alist
        (append (list
                 '(min-height . 1)  '(height . 32)
                 '(min-width  . 1)  '(width  . 96)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 0)
                 '(left-fringe . 8)
                 '(right-fringe . 8)
                 '(undecorated-round . t) ;; emacs-plu@29 only
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))
  
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)

  ;; Vertical window divider
  (setq window-divider-default-right-width 2
        window-divider-default-bottom-width 1
        window-divider-default-places t)
  
  (window-divider-mode 1))

(defun liminal-ux-settings ()
  "Apply the preferred Liminal settings for user experience.

- Turn off all startup screen related features.
- Remove scratch message and set initial scratch mode to text.
- Turn off file dialog, dialog boxes, and pop-up windows.
- Set fill column to 80 characters, turn off auto fill mode."
  
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t)

  (setq initial-scratch-message nil
        initial-buffer-choice nil
        initial-major-mode 'text-mode)

  (setq use-file-dialog nil
        use-dialog-box nil
        pop-up-windows nil)

  (setq fill-column 80)
  (auto-fill-mode nil))

(defun liminal-mode ()
  "Defaults UI/UX settings for liminal (optional)."
  (interactive)

  (when liminal-manage-cursor
    (message "Turning on liminal cursor mode")
    (liminal-font-settings))

  (when liminal-manage-fonts
    (message "Turning on liminal font mode")
    (liminal-font-settings))

  (when liminal-manage-ui
    (message "Turning on liminal UI mode")
    (liminal-ui-settings))

  (when liminal-manage-ux
    (message "Turning on liminal UX mode")
    (liminal-ux-settings)))

(provide 'liminal-theme-utils)
;;; liminal-theme-utils.el ends here

