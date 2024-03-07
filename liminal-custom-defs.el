;;; liminal-custom-defs.el --- Custom/Face defs for liminal theme -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup liminal nil
  "Customization settings for the Liminal meta-package."
  :group 'convenience)

(defgroup liminal-theme nil
  "Liminal theme."
  :group 'liminal)

(defcustom liminal-window-divider-show nil
  "Whether to show the vertical window-divider."
  :type 'boolean :group 'liminal-theme)

(defgroup liminal-theme-light nil
  "Light color palette."
  :group 'liminal-theme)

(defgroup liminal-theme-dark nil
  "Dark color palette."
  :group 'liminal-theme)

(defgroup liminal-theme-fonts nil
  "Font stack."
  :group 'liminal-theme)

(defcustom liminal-fonts-use nil
  "Whether to use font stack."
  :type 'boolean :group 'liminal-theme-fonts)

(defcustom liminal-font-size 14
  "Font size to use for liminal font stack."
  :type 'integer :group 'liminal-theme-fonts)

(defface liminal-mono
  '((t (:weight normal :family "JuliaMono")))
  "Default monospaced font (Julia Mono)."
  :group 'liminal-theme-fonts)

(defface liminal-mono-alt
  '((t (:family "Fire Code"
        :weight light)))
  "Alternative monospaced font (Fira Code Light)."
  :group 'liminal-theme-fonts)

(defface liminal-sans
  '((t (:family "Iosevka Aile"
        :weight medium)))
  "Default proportional sans font (Iosevka Aile)."
  :group 'liminal-theme-fonts)

(defface liminal-serif
  '((t (:family "Iosevka Etoile"
        :weight medium)))
  "Default proportional serif font (Iosevka Etoile)."
  :group 'liminal-theme-fonts)

(defface liminal-italic
  '((t (:family "JuliaMono"
        :slant italic)))
  "Default italic font (JuliaMono)."
  :group 'liminal-theme-fonts)

(defcustom liminal-white "#FAFAFA"
  "Default background color."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-off-white "#E2E2E2"
  "Lighter grey."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-black "#222222"
  "Default foreground color."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-off-black "#3e3e3e"
  "Default foreground color."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-light-grey "#9EA6AA"
  "Darker grey."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-dark-grey "#6f6f6f"
  "Darker grey."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-light-blue "#00749d"
  "Primary blue."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-light-cyan  "#2A789E"
  "Primary cyan."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-light-green "#007a7b"
  "Primary green."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-light-red "#db0072"
  "Primary red."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-light-magenta "#d2008f"
  "Primary magenta."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-light-yellow "#926700"
  "Primary yellow."
  :type 'color :group 'liminal-theme-light)

(defcustom liminal-dark-blue "#0092c5"
  "Primary blue."
  :type 'color :group 'liminal-theme-dark)

(defcustom liminal-dark-cyan  "#00999a"
  "Primary cyan."
  :type 'color :group 'liminal-theme-dark)

(defcustom liminal-dark-green "#00a21f"
  "Primary green."
  :type 'color :group 'liminal-theme-dark)

(defcustom liminal-dark-red "#ff3b8d"
  "Primary red."
  :type 'color :group 'liminal-theme-dark)

(defcustom liminal-dark-magenta "#ff426c"
  "Primary magenta."
  :type 'color :group 'liminal-theme-dark)

(defcustom liminal-dark-yellow "#b98300"
  "Primary yellow."
  :type 'color :group 'liminal-theme-dark)

(defface liminal-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces.  This
can be realized (for example) by setting an intense background
color, typically a shade of red.  It must be used scarcely."
  :group nil)

(defface liminal-critical-i nil
  "Critical face inverted."
  :group nil)

(defface liminal-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface liminal-popout-i nil
  "Popout face inverted."
  :group nil)

(defface liminal-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold).  It is generally used for titles, keywords,
directory, etc."
  :group nil)

(defface liminal-strong-i nil
  "Strong face inverted."
  :group nil)

(defface liminal-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface liminal-salient-i nil
  "Strong face inversed."
  :group nil)

(defface liminal-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default.  It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface liminal-faded-i nil
  "Faded face inverted."
  :group nil)

(defface liminal-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface liminal-subtle-i nil
  "Subtle face inverted."
  :group nil)

(defface liminal-default nil
  "Default face."
  :group nil)

(defface liminal-default-i nil
  "Default face inverted."
  :group nil)

(provide 'liminal-custom-defs)
;;; liminal-custom-defs.el ends here

