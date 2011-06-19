;;; lisppaste-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (lisppaste lisppaste-display-supported-channels
;;;;;;  lisppaste-annotate lisppaste-dwim lisppaste-create-new-annotation
;;;;;;  lisppaste-create-new-paste lisppaste-list-recent-pastes lisppaste-list-paste-annotations
;;;;;;  lisppaste-display-paste lisppaste-paste-region lisppaste-list-pastes)
;;;;;;  "lisppaste" "lisppaste.el" (19922 61759))
;;; Generated autoloads from lisppaste.el

(autoload 'lisppaste-list-pastes "lisppaste" "\
Fetch the most recent N pastes.

If START is non-nil return the most recent N pastes from the STARTth
paste.
If CHANNEL is non-nil, only return pastes from that channel.

\(fn N &optional START CHANNEL)" nil nil)

(autoload 'lisppaste-paste-region "lisppaste" "\
Send the region between BEG and END as a paste.

\(fn BEG END)" t nil)

(autoload 'lisppaste-display-paste "lisppaste" "\
Display PASTE.

If N is non-nil, display PASTE's Nth annotation.

\(fn PASTE &optional N)" t nil)

(autoload 'lisppaste-list-paste-annotations "lisppaste" "\
List PASTE's annotations.

\(fn PASTE)" t nil)

(autoload 'lisppaste-list-recent-pastes "lisppaste" "\
List the most recent N pastes.

If START is non-nil, list the most recent N pastes prior to and
including START.
If CHANNEL is non-nil, only list pastes for that channel.

\(fn N &optional START CHANNEL)" t nil)

(autoload 'lisppaste-create-new-paste "lisppaste" "\
Interactively create a new paste.

CHANNEL, NICK and TITLE are defaults for the paste's channel, nick
and title arguments respectively.

\(fn &optional CHANNEL NICK TITLE)" t nil)

(autoload 'lisppaste-create-new-annotation "lisppaste" "\
Interactively annotate a paste.

CHANNEL, NICK, TITLE and N are defaults for the annotations's
channel, nick, title, and paste to annotate respectively.

\(fn &optional CHANNEL NICK TITLE N)" t nil)

(autoload 'lisppaste-dwim "lisppaste" "\
Display either the paste or annotation at `point'.

\(fn)" t nil)

(autoload 'lisppaste-annotate "lisppaste" "\
Annotate the paste at `point'.

\(fn)" t nil)

(autoload 'lisppaste-display-supported-channels "lisppaste" "\
Display the channels that lisppaste is running in.

As a side-effect, this updates the channel list stored in the
variable `lisppaste-channels'.

\(fn)" t nil)

(autoload 'lisppaste "lisppaste" "\
Top-level interface to lisppaste.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("lisppaste-pkg.el") (19922 61759 858991))

;;;***

(provide 'lisppaste-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lisppaste-autoloads.el ends here
