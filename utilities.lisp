;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :art-social-graph)

(defun get-http-page (url parameters)
  "Get the page with the given parameters. Returns mpty string for failure."
  (multiple-value-bind (body result-code)
      (drakma:http-request url
			   :method :get
			   :parameters parameters)
    (if (eq result-code 200)
	body
	"")))

(defun nbsp-to-space (text)
  (cl-ppcre:regex-replace-all " " text " "))