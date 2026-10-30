;;; r2-uploader.el --- Upload files to an r2-uploader instance -*- lexical-binding: t; -*-

;; Author: hushin
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, org, hypermedia
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Upload a file with POST /api/files and insert the returned public URL
;; into the current buffer.  The token is read from auth-source, so it never
;; appears on the curl command line.
;;
;; M-x r2-uploader-yank-image  upload the clipboard image
;; M-x r2-uploader-insert-file upload a file chosen with read-file-name

;;; Code:

(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'url-parse)

(defgroup r2-uploader nil
  "Upload files to an r2-uploader instance."
  :group 'tools
  :prefix "r2-uploader-")

(defcustom r2-uploader-base-url "http://localhost:8787"
  "Base URL of the r2-uploader deployment."
  :type 'string
  :group 'r2-uploader)

(defcustom r2-uploader-curl-program "curl"
  "Name or path of the curl executable."
  :type 'string
  :group 'r2-uploader)

(defcustom r2-uploader-clipboard-command
  (cond
   ((eq system-type 'windows-nt)
    ;; No `$' and the whole script in double quotes, so cmd and bash both pass it through unchanged.
    "powershell -NoProfile -Command \"Add-Type -AssemblyName System.Windows.Forms; [Windows.Forms.Clipboard]::GetImage().Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)\"")
   ((executable-find "wl-paste")
    "wl-paste --type image/png > %s")
   ((executable-find "xclip")
    "xclip -selection clipboard -t image/png -o > %s")
   ((executable-find "pngpaste")
    "pngpaste %s")
   (t
    "xclip -selection clipboard -t image/png -o > %s"))
  "Shell command that saves the clipboard image to the path substituted for %s.
On platforms other than Windows the path is shell-quoted before
substitution.  On Windows it is inserted verbatim, so the command should
quote %s itself (as the default PowerShell command does)."
  :type 'string
  :group 'r2-uploader)

(defun r2-uploader--parsed-base-url ()
  "Return the parsed `r2-uploader-base-url', validating it."
  (let ((parsed (url-generic-parse-url r2-uploader-base-url)))
    (unless (and (url-host parsed)
                 (member (url-type parsed) '("http" "https")))
      (user-error "r2-uploader: invalid base URL: %s" r2-uploader-base-url))
    parsed))

(defun r2-uploader--authinfo-hint (host port)
  "Return an authinfo line for HOST and optional PORT."
  (format "machine %s%s login r2-uploader password <token>"
          host
          (if port (format " port %s" port) "")))

(defun r2-uploader--auth-entries (host port)
  "Search auth-source for HOST and optional PORT."
  (let ((query (list :host host :user "r2-uploader" :require '(:secret))))
    (or (and port
             (apply #'auth-source-search
                    (append query (list :port port))))
        (apply #'auth-source-search query))))

(defun r2-uploader--token ()
  "Return the upload token for `r2-uploader-base-url'."
  (let* ((parsed (r2-uploader--parsed-base-url))
         (host (url-host parsed))
         (port (url-portspec parsed))
         (entry (car (r2-uploader--auth-entries host port)))
         (secret (plist-get entry :secret)))
    (unless entry
      (user-error "r2-uploader: no token for %s; add `%s' to ~/.authinfo.gpg"
                  host (r2-uploader--authinfo-hint host port)))
    (when (functionp secret)
      (setq secret (funcall secret)))
    (unless (and (stringp secret) (not (string-empty-p secret)))
      (user-error "r2-uploader: auth-source has no password for %s" host))
    secret))

(defun r2-uploader--response-summary (body)
  "Return a short, non-empty summary of BODY for error messages."
  (let ((trimmed (string-trim body)))
    (if (string-empty-p trimmed)
        "<empty response>"
      (truncate-string-to-width trimmed 1000 nil nil "..."))))

(defun r2-uploader--request-url ()
  "Return the upload endpoint URL."
  (concat (string-trim-right r2-uploader-base-url "/+") "/api/files"))

(defun r2-uploader-upload-file (file)
  "Upload FILE and return its public URL.
Signal a `user-error' when the request fails, including the HTTP status
and response body."
  (interactive "fFile: ")
  (let ((file (expand-file-name file)))
    (unless (file-regular-p file)
      (user-error "r2-uploader: not a regular file: %s" file))
    (unless (file-readable-p file)
      (user-error "r2-uploader: cannot read file: %s" file))
    (unless (executable-find r2-uploader-curl-program)
      (user-error "r2-uploader: curl executable not found: %s"
                  r2-uploader-curl-program))
    (let ((token (r2-uploader--token))
          (header-file nil)
          (body-file nil)
          (stderr-file nil)
          (status-buffer nil))
      (unwind-protect
          (progn
            (setq header-file (make-temp-file "r2-uploader-header-"))
            (setq body-file (make-temp-file "r2-uploader-body-"))
            (setq stderr-file (make-temp-file "r2-uploader-stderr-"))
            (setq status-buffer (generate-new-buffer " *r2-uploader-curl*"))
            (with-temp-file header-file
              (insert "Authorization: Bearer " token "\n"))
            (let* ((args (list "-sS"
                               "--fail-with-body"
                               "--request" "POST"
                               "--header" (concat "@" header-file)
                               "--form" (concat "file=@" file)
                               "--output" body-file
                               "--write-out" "%{http_code}"
                               (r2-uploader--request-url)))
                   (exit (apply #'call-process
                                r2-uploader-curl-program nil
                                (list status-buffer stderr-file) nil args))
                   (status (with-current-buffer status-buffer
                             (string-to-number
                              (string-trim (buffer-string)))))
                   (body (with-temp-buffer
                           (insert-file-contents body-file)
                           (buffer-string)))
                   (stderr (with-temp-buffer
                             (insert-file-contents stderr-file)
                             (string-trim (buffer-string)))))
              (unless (and (integerp exit) (zerop exit))
                (user-error
                 "r2-uploader: upload failed (HTTP %s, curl exit %s): %s%s"
                 status exit (r2-uploader--response-summary body)
                 (if (string-empty-p stderr)
                     ""
                   (concat " (" stderr ")"))))
              (unless (eql status 200)
                (user-error "r2-uploader: upload failed (HTTP %s): %s"
                            status (r2-uploader--response-summary body)))
              (let* ((response
                      (condition-case nil
                          (json-parse-string body :object-type 'alist)
                        (error
                         (user-error
                          "r2-uploader: invalid JSON (HTTP %s): %s"
                          status (r2-uploader--response-summary body)))))
                     (url (alist-get 'url response)))
                (unless (stringp url)
                  (user-error "r2-uploader: response has no url: %s"
                              (r2-uploader--response-summary body)))
                url)))
        (when header-file
          (ignore-errors (delete-file header-file)))
        (when body-file
          (ignore-errors (delete-file body-file)))
        (when stderr-file
          (ignore-errors (delete-file stderr-file)))
        (when (buffer-live-p status-buffer)
          (kill-buffer status-buffer))))))

(defun r2-uploader--save-clipboard-image (file)
  "Save the clipboard image to FILE, signalling a `user-error' on failure."
  (let* ((path (if (eq system-type 'windows-nt)
                   file
                 (shell-quote-argument file)))
         (command (format r2-uploader-clipboard-command path)))
    (with-temp-buffer
      (let ((exit (call-process-shell-command
                   command nil (list (current-buffer) t) t)))
        (unless (and (integerp exit) (zerop exit))
          (user-error "r2-uploader: clipboard command failed (exit %s): %s"
                      exit (r2-uploader--response-summary
                            (buffer-string))))))
    (unless (and (file-exists-p file)
                 (> (file-attribute-size (file-attributes file)) 0))
      (user-error "r2-uploader: clipboard command produced no image"))))

(defun r2-uploader--insert-link (url)
  "Insert URL at point, as an Org link when in `org-mode'."
  (if (derived-mode-p 'org-mode)
      (progn
        (insert (format "[[%s]]" url))
        (when (fboundp 'org-redisplay-inline-images)
          (org-redisplay-inline-images)))
    (insert url)))

(defun r2-uploader-insert-file (file)
  "Upload FILE and insert its public URL at point."
  (interactive "fFile: ")
  (r2-uploader--insert-link (r2-uploader-upload-file file)))

(defun r2-uploader-yank-image ()
  "Upload the clipboard image and insert its public URL at point."
  (interactive)
  (let ((file (make-temp-file "r2-uploader-clipboard-" nil ".png")))
    (unwind-protect
        (progn
          (r2-uploader--save-clipboard-image file)
          (r2-uploader--insert-link (r2-uploader-upload-file file)))
      (ignore-errors (delete-file file)))))

(provide 'r2-uploader)
;;; r2-uploader.el ends here
