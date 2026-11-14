;;; acp-image.el --- Bounded inline ACP images -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared image conversion for ACP frontends and adapters.  Images travel as
;; bytes, never as paths to fetch or open on the agent's workspace host.

;;; Code:
(require 'image)
(require 'map)
(require 'subr-x)

(defgroup acp-image nil "Inline images in ACP prompts." :group 'applications)

(defcustom acp-image-max-bytes (* 20 1024 1024)
  "Maximum decoded bytes accepted in one inline image."
  :type 'natnum :group 'acp-image)

(defconst acp-image-mime-types
  '(("image/png" . png) ("image/jpeg" . jpeg)
    ("image/webp" . webp) ("image/gif" . gif))
  "Supported raster MIME types and their Emacs image types.")

(defun acp-image-max-encoded-length ()
  "Return the maximum Base64 length of one supported attachment."
  (* 4 (/ (+ acp-image-max-bytes 2) 3)))

(defun acp-image-from-data (mime data)
  "Return an ACP image block for MIME and binary DATA.
Reject empty, oversized or mismatched data before storing it in a draft."
  (let ((type (cdr (assoc mime acp-image-mime-types))))
    (unless type (user-error "Unsupported image type: %s" mime))
    (unless (and (stringp data) (> (length data) 0)
                 (<= (string-bytes data) acp-image-max-bytes))
      (user-error "Image is empty or exceeds the %d MiB limit"
                  (/ acp-image-max-bytes 1024 1024)))
    (unless (eq type (image-type-from-data data))
      (user-error "Image data does not match %s" mime))
    `((type . "image") (mimeType . ,mime)
      (data . ,(base64-encode-string data t)))))

(defun acp-image-data (content)
  "Validate ACP image CONTENT and return its decoded bytes."
  (let ((encoded (map-elt content 'data))
        (mime (map-elt content 'mimeType)))
    (unless (and (equal (map-elt content 'type) "image")
                 (assoc mime acp-image-mime-types)
                 (stringp encoded)
                 (<= (length encoded) (acp-image-max-encoded-length)))
      (user-error "Invalid or oversized inline image"))
    (let ((data (condition-case nil
                    (base64-decode-string encoded)
                  (error (user-error "Invalid Base64 image data")))))
      (unless (equal encoded (map-elt (acp-image-from-data mime data) 'data))
        (user-error "Invalid Base64 image data"))
      data)))

(defun acp-image-to-data-url (content)
  "Return a validated data URL for ACP image CONTENT."
  (acp-image-data content)
  (concat "data:" (map-elt content 'mimeType) ";base64,"
          (map-elt content 'data)))

(defun acp-image-from-data-url (url)
  "Return an ACP image for inline URL, or nil for other URL schemes.
Never fetch resources or read files while restoring conversation history."
  (when (and (stringp url)
             (string-match "\\`data:\\(image/[a-z]+\\);base64," url))
    (let ((content `((type . "image") (mimeType . ,(match-string 1 url))
                     (data . ,(substring url (match-end 0))))))
      (acp-image-data content)
      content)))

(provide 'acp-image)
;;; acp-image.el ends here
