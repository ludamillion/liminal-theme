;;; liminal-theme.el --- N Λ N O theme -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Free Software Foundation, Inc.

;; Maintainer: Luke D. Inglis <ld.inglis@gmail.com>
;; URL: https://github.com/ludamillion/nano-theme
;; Version: 0.3.4
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
;; lovely Modus themes and overall.  It is a fork of Nicolas
;; Rougier's N Λ N O theme and aims to mostly follow the same
;; design goals.
;;
;; References
;; - https://protesilaos.com/emacs/modus-themes
;; - https://github.com/rougier/nano-theme
;;
;; A theme is fully defined by a set of (1+6) faces as explained in
;; "On the Design of Text Editors" / https://arxiv.org/abs/2008.06030
;;

;;; Code:
(require 'liminal-theme-support)

;;;###autoload
(deftheme liminal "λinimal theme.")

(liminal-theme 'liminal (frame-parameter (selected-frame) 'background-mode))

(provide-theme 'liminal)
;;; liminal-theme.el ends here
